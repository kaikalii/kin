#include "list.h"

int main(int argc, char** argv) {
    tgc_start(&noot_gc, &argc);
    NootList list1 = noot_list_push(new_list_inner(), new_int(5));
    NootList list2 = noot_list_push(list1, new_int(6));
    NootValue val0 = noot_list_get(list1, 0);
    NootValue val1_from_1 = noot_list_get(list1, 1);
    NootValue val1_from_2 = noot_list_get(list2, 1);
    noot_println(1, &val0);
    noot_println(1, &val1_from_1);
    noot_println(1, &val1_from_2);
    tgc_stop(&noot_gc);
    return 0;
}