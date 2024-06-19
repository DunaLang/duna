#include "hashmap_utils.h"

size_t hash(char *key)
{
    size_t hashedKey = 0;

    int i = 1;
    while (key[0] != '\0')
    {
        hashedKey = (hashedKey + key[0] * i) % HASH_SIZE;
        i++;
        key++;
    }
    return hashedKey;
}
