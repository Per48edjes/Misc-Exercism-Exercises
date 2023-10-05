#include "linked_list.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct list_node
{
    struct list_node *prev, *next;
    ll_data_t data;
};

struct list
{
    struct list_node *first, *last;
    size_t length;
};

// constructs a new (empty) list
struct list* list_create(void)
{
    struct list* list = calloc(1, sizeof(struct list));
    return list;
}

// NOTE: Experimenting with `inline`
static inline bool list_is_empty(struct list* list)
{
    return (list->length == 0);
}

// counts the items on a list
size_t list_count(const struct list* list) { return list->length; }

// inserts item at back of a list
void list_push(struct list* list, ll_data_t item_data)
{
    struct list_node* node = malloc(1 * sizeof(struct list_node));
    node->data = item_data;
    node->next = NULL;

    if (list->length == 0)
    {
        node->prev = NULL;
        list->first = node;
        list->last = node;
    }
    else
    {
        list->last->next = node;
        node->prev = list->last;
        list->last = node;
    }

    list->length++;
}

// removes item from back of a list
ll_data_t list_pop(struct list* list)
{
    if (list_is_empty(list))
    {
        fprintf(stderr, "list_pop: popping from empty list");
        exit(EXIT_FAILURE);
    }

    ll_data_t data = list->last->data;

    if (list->length == 1)
    {
        free(list->last);
        list->last = NULL;
        list->first = NULL;
    }
    else
    {
        struct list_node* new_last = list->last->prev;
        new_last->next = NULL;
        free(list->last);
        list->last = new_last;
    }

    list->length--;
    return data;
}

// inserts item at front of a list
void list_unshift(struct list* list, ll_data_t item_data)
{
    struct list_node* node = malloc(1 * sizeof(struct list_node));
    node->data = item_data;
    node->prev = NULL;

    if (list->length == 0)
    {
        node->next = NULL;
        list->first = node;
        list->last = node;
    }
    else
    {
        list->first->prev = node;
        node->next = list->first;
        list->first = node;
    }

    list->length++;
}

// removes item from front of a list
ll_data_t list_shift(struct list* list)
{
    if (list_is_empty(list))
    {
        fprintf(stderr, "list_shift: removing from empty list");
        exit(EXIT_FAILURE);
    }

    ll_data_t data = list->first->data;

    if (list->length == 1)
    {
        free(list->first);
        list->first = NULL;
        list->last = NULL;
    }
    else
    {
        struct list_node* new_first = list->first->next;
        new_first->prev = NULL;
        free(list->first);
        list->first = new_first;
    }

    list->length--;
    return data;
}

// deletes a node that holds the matching data
void list_delete(struct list* list, ll_data_t data)
{
    if (list_is_empty(list))
    {
        return;
    }

    if (list->first->data == data)
    {
        list_shift(list);
        return;
    }

    if (list->length == 1)
    {
        return;
    }

    for (struct list_node* p = list->first->next; p != list->last; p = p->next)
    {
        if (p->data == data)
        {
            struct list_node* left = p->prev;
            struct list_node* right = p->next;
            free(p);
            left->next = right;
            right->prev = left;
            list->length--;
            return;
        }
    }

    if (list->last->data == data)
    {
        list_pop(list);
        return;
    }
}

// destroys an entire list
// list will be a dangling pointer after calling this method on it
void list_destroy(struct list* list)
{
    while (!list_is_empty(list))
    {
        list_pop(list);
    }

    free(list);
}
