my_init xs = take (length xs - 1) xs

my_init2 xs = reverse (drop 1 (reverse xs))