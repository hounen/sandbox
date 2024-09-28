def test10_func():
    tmp = test11_func()
    tmp = test12_func()
    return 'tmp'

def test11_func():
    tmp = test13_func()
    return 'tmp'

def test12_func():
    return 'tmp'

def test13_func():
    tmp = test14_func()
    return 'tmp'

def test14_func():
    return 'tmp'