{-# LANGUAGE FlexibleInstances #-}

module FlashDB.KVDB
  ( KVDB,
    open,
    set,
    get,
    delete,
    sectionSize,
    maxSize,
    remainSize,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCStringLen)
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "fdb_kvdb_new"
  fdb_kvdb_new :: CString -> CString -> CUInt -> CUInt -> IO KVDBHandle

foreign import ccall "&fdb_kvdb_delete"
  fdb_kvdb_delete :: FunPtr (KVDBHandle -> IO ())

foreign import ccall "fdb_kv_set_data"
  fdb_kv_set_data :: KVDBHandle -> CString -> Ptr () -> CSize -> IO Int

foreign import ccall "fdb_kv_get_data"
  fdb_kv_get_data :: KVDBHandle -> CString -> Ptr GetInfo -> IO Int

foreign import ccall "fdb_kv_del"
  fdb_kv_del :: KVDBHandle -> CString -> IO Int

foreign import ccall "fdb_sec_size"
  fdb_sec_size :: KVDBHandle -> IO CUInt

foreign import ccall "fdb_max_size"
  fdb_max_size :: KVDBHandle -> IO CUInt

foreign import ccall "fdb_kvdb_remain_size"
  fdb_kvdb_remain_size :: KVDBHandle -> IO CUInt

foreign import ccall "stdlib free"
  c_free :: Ptr () -> IO ()

data GetInfo = GetInfo
  { get_value :: Ptr (),
    get_length :: CSize
  }
  deriving (Show, Eq)

type KVDBStruct = ()

type KVDBHandle = Ptr KVDBStruct

newtype KVDB = KVDB {handle :: ForeignPtr KVDBStruct}

class Value a where
  set' :: KVDBHandle -> CString -> a -> IO Int
  get' :: KVDBHandle -> CString -> IO (Maybe a)

instance Value String where
  set' db k v = do
    cStrLen <- newCStringLen v
    setDBData db k cStrLen
  get' db k = do
    str <- getDBData db k
    res <- mapM peekCStringLen str
    _ <- mapM (c_free . castPtr . fst) str
    return res

instance Value BS.ByteString where
  set' db k v =
    unsafeUseAsCStringLen v $ \cStrLen ->
      setDBData db k cStrLen
  get' db k = do
    res <- getDBData db k
    mapM unsafePackCStringLen res

instance Value BSL.ByteString where
  set' db k v = set' db k (BSL.toStrict v)
  get' db k = do
    v <- get' db k
    return (BSL.fromStrict <$> v)

open :: String -> FilePath -> Int -> Int -> IO KVDB
open name path sectorSize dbSize = do
  c_name <- newCString name
  c_path <- newCString path
  db <-
    fdb_kvdb_new
      c_name
      c_path
      (fromIntegral sectorSize)
      (fromIntegral dbSize)
  db' <- newForeignPtr fdb_kvdb_delete db
  return KVDB {handle = db'}

set :: Value a => KVDB -> String -> a -> IO ()
set db k v = db <&&> \db' -> do
  c_k <- newCString k
  err <- set' db' c_k v
  case err of
    0 -> return ()
    x -> error $ "KVDB set error: " ++ show x

get :: Value a => KVDB -> String -> IO (Maybe a)
get db k = db <&&> \db' -> do
  c_k <- newCString k
  get' db' c_k

delete :: KVDB -> String -> IO Bool
delete db k = db <&&> \db' -> do
  c_k <- newCString k
  res <- fdb_kv_del db' c_k
  return (res == 0)

sectionSize :: KVDB -> IO Int
sectionSize = (<&&> (fromIntegral <$>) . fdb_sec_size)

maxSize :: KVDB -> IO Int
maxSize = (<&&> (fromIntegral <$>) . fdb_max_size)

remainSize :: KVDB -> IO Int
remainSize = (<&&> (fromIntegral <$>) . fdb_kvdb_remain_size)

instance Storable GetInfo where
  alignment _ = 8
  sizeOf _ = 16
  peek ptr =
    GetInfo
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr (alignment nullPtr)
  poke ptr (GetInfo p s) = do
    pokeByteOff ptr 0 p
    pokeByteOff ptr (alignment nullPtr) s

nullInfo :: GetInfo
nullInfo = GetInfo nullPtr 0

(<&&>) :: KVDB -> (KVDBHandle -> IO a) -> IO a
(<&&>) db = withForeignPtr (handle db)

infixl 9 <&&>

setDBData :: KVDBHandle -> CString -> CStringLen -> IO Int
setDBData db k (cstr, len) =
  fdb_kv_set_data db k (castPtr cstr) (fromIntegral len)

getDBData :: KVDBHandle -> CString -> IO (Maybe CStringLen)
getDBData db k = allocaBytes (sizeOf nullInfo) $ \ptr -> do
  res <- fdb_kv_get_data db k ptr
  value <- peek ptr
  let sptr = castPtr $ get_value value
      len = fromIntegral $ get_length value
  if res == 0
    then return $ Just (sptr, len)
    else return Nothing
