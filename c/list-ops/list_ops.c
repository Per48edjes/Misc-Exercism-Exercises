#include "list_ops.h"
#include <string.h>


static list_t *create_list(size_t length)
{
    list_t *list = malloc(sizeof(list_t) + length * sizeof(list_element_t));
    if (!list)
    {
        return NULL;
    }

    list->length = length;

    return list;
}

list_t *new_list(size_t length, list_element_t elements[])
{
    list_t *list = create_list(length);
    if (!list)
    {
        return NULL;
    }
    memcpy(list->elements, elements, length * sizeof(list_element_t));

    return list;
}

list_t *append_list(list_t *list1, list_t *list2)
{
    list_t *concat_list = create_list(list1->length + list2->length);
    if (!concat_list)
    {
        return NULL;
    }

    memcpy(concat_list->elements, list1->elements,
           list1->length * sizeof(list_element_t));
    memcpy(&concat_list->elements[list1->length], list2->elements,
           list2->length * sizeof(list_element_t));

    return concat_list;
}

list_t *filter_list(list_t *list, bool (*filter)(list_element_t))
{
    list_t *filter_list = create_list(list->length);
    if (!filter_list)
    {
        return NULL;
    }

    filter_list->length = 0;
    for (size_t i = 0; i < list->length; ++i)
    {
        if ((*filter)(list->elements[i]))
        {
            filter_list->elements[filter_list->length++] = list->elements[i];
        }
    }

    return filter_list;
}

size_t length_list(list_t *list)
{
    return list->length;
}

list_t *map_list(list_t *list, list_element_t (*map)(list_element_t))
{
    list_t *map_list = create_list(list->length);
    if (!map_list)
    {
        return NULL;
    }

    for (size_t i = 0; i < list->length; ++i)
    {
        map_list->elements[i] = (*map)(list->elements[i]);
    }

    return map_list;
}

list_element_t foldl_list(list_t           *list,
                          list_element_t    initial,
                          list_element_t ( *foldl )(list_element_t,
                                                    list_element_t))
{
    list_element_t accum = initial;
    for (size_t i = 0; i < list->length; ++i)
    {
        accum = (*foldl)(accum, list->elements[i]);
    }

    return accum;
}

list_element_t foldr_list(list_t           *list,
                          list_element_t    initial,
                          list_element_t ( *foldr )(list_element_t,
                                                    list_element_t))
{
    list_element_t accum = initial;
    for (size_t i = list->length; i > 0; --i)
    {
        accum = (*foldr)(list->elements[i - 1], accum);
    }

    return accum;
}

list_t *reverse_list(list_t *list)
{
    list_t *reverse_list = create_list(list->length);
    if (!reverse_list)
    {
        return NULL;
    }

    for (size_t i = 0; i < list->length; ++i)
    {
        reverse_list->elements[i] = list->elements[list->length - 1 - i];
    }

    return reverse_list;
}

void delete_list(list_t *list)
{
    free(list);
}
