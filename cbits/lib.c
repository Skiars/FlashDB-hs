#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <flashdb.h>

struct get_info {
    const void *value;
    size_t length;
};

fdb_kvdb_t fdb_kvdb_new(const char *name, const char *path, uint32_t sector_size, uint32_t db_size)
{
    bool file_mode = true;
    fdb_kvdb_t db = malloc(sizeof(struct fdb_kvdb));
    memset(db, 0, sizeof(struct fdb_kvdb));
    fdb_kvdb_control(db, FDB_KVDB_CTRL_SET_SEC_SIZE, &sector_size);
    fdb_kvdb_control(db, FDB_KVDB_CTRL_SET_MAX_SIZE, &db_size);
    /* enable file mode */
    fdb_kvdb_control(db, FDB_KVDB_CTRL_SET_FILE_MODE, &file_mode);
    fdb_err_t err = fdb_kvdb_init(db, name, path, NULL, NULL);
    if (err == FDB_NO_ERR)
        return db;
    free(db);
    return NULL;
}

void fdb_kvdb_delete(fdb_kvdb_t db)
{
    fdb_kvdb_deinit(db);
    free(db);
}

uint32_t fdb_sec_size(fdb_kvdb_t db)
{
    return db->parent.sec_size;
}

uint32_t fdb_max_size(fdb_kvdb_t db)
{
    return db->parent.max_size;
}

uint32_t fdb_kvdb_remain_size(fdb_kvdb_t db)
{
    return db->cur_sector.remain;
}

fdb_err_t fdb_kv_set_data(fdb_kvdb_t db, const char *key, const void *data, size_t len)
{
    struct fdb_blob blob;
    return fdb_kv_set_blob(db, key, fdb_blob_make(&blob, data, len));
}

void fdb_kv_get_data(fdb_kvdb_t db, const char *key, struct get_info *res)
{
    struct fdb_blob blob;
    size_t size = fdb_kv_get_blob(db, key, &blob);
    res->value = blob.buf;
    res->length = blob.saved.len;
}
