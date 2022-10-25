#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <flashdb.h>

struct get_info {
    const void *value;
    size_t length;
};

static char* m_strdup(const char *s)
{
    size_t size = strlen(s);
    char *p = malloc(size + 1);
    return memcpy(p, s, size + 1);
}

fdb_kvdb_t fdb_kvdb_new(const char *name, const char *path, uint32_t sector_size, uint32_t db_size)
{
    bool file_mode = true;
    fdb_kvdb_t db = malloc(sizeof(struct fdb_kvdb));
    memset(db, 0, sizeof(struct fdb_kvdb));
    fdb_kvdb_control(db, FDB_KVDB_CTRL_SET_SEC_SIZE, &sector_size);
    fdb_kvdb_control(db, FDB_KVDB_CTRL_SET_MAX_SIZE, &db_size);
    /* enable file mode */
    fdb_kvdb_control(db, FDB_KVDB_CTRL_SET_FILE_MODE, &file_mode);
    fdb_err_t err = fdb_kvdb_init(db, name, m_strdup(path), NULL, NULL);
    if (err == FDB_NO_ERR)
        return db;
    free(db);
    return NULL;
}

void fdb_kvdb_delete(fdb_kvdb_t db)
{
    const char *s = db->parent.storage.dir;
    fdb_kvdb_deinit(db);
    free(db);
    free((void *)s);
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

int fdb_kv_get_data(fdb_kvdb_t db, const char *key, struct get_info *res)
{
    struct fdb_kv kv;
    struct fdb_blob blob;
    fdb_kv_t kvp = fdb_kv_get_obj(db, key, &kv);
    if (!kvp)
        return -1;
    void *buf = malloc(kvp->len);
    fdb_blob_read(&db->parent,
                  fdb_kv_to_blob(kvp, fdb_blob_make(&blob, buf, kvp->len)));
    res->value = blob.buf;
    res->length = blob.saved.len;
    return 0;
}
