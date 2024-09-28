def fnc_a():
    print('a')
    fnc_b()
 
def fnc_b():
    print('b')
 
if __name__ == '__main__':
    fnc_a()