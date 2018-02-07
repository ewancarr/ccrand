import itertools

clusters = 8

test = ["".join(seq) for seq in itertools.product("01", repeat=4)]

params = (prod for prod in itertools.product("01", repeat=2) if prod[2] <= prod[1])

params = itertools.product(range(10,41,2), range(10,41,2), range(0, 2), range(5, 31, 5))

params = list(itertools.product((0, 1), repeat = 4))

[sum(p) for p in params]

list(map(sum, params))

filtered_params = itertools.ifilter
    (lambda (a1, a2, b1, b2): a1 <= a2 and (b1 != 0 or b2 == 5), params)


params = itertools.product((0, 1), repeat = 8)

test = (prod for prod in params if sum(prod) == 2)

valid_options = (prod for prod in itertools.product((0, 1), repeat = 30) if sum(prod) == 13)

len(list(valid_options))

all_options = itertools.product((0, 1), repeat = 8)


atest = (prod for prod in itertools.product((0, 1), repeat = 8) if sum(prod) == 2)



correct_allow = lambda x: sum(x) == 4

all_options = itertools.product((0, 1), repeat = 60)
list(itertools.filterfalse(lambda x: sum(x) == 15, all_options))
