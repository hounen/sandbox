import sample02

def test01_func():
    tmp = test02_func()
    tmp = test03_func()
    tmp = test04_func()
    tmp = sample02.test10_func()
    return 'tmp'

def test02_func():
    tmp = test06_func()
    return 'tmp'

def test03_func():
    return 'tmp'

def test04_func():
    tmp = test05_func()
    return 'tmp'

def test05_func():
    tmp_class = Test01Class()
    tmp = tmp_class.test07_func()
    return 'tmp'

def test06_func():
    return 'tmp'

class Test01Class:
    def test07_func(self):
        tmp = self.test08_func()
        tmp = self.test09_func()
        return 'tmp'

    def test08_func(self):
        return 'tmp'

    def test09_func(self):
        return 'tmp'

if __name__ == "__main__":
    tmp = test01_func()