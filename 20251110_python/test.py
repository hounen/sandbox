import importlib
import glob

from test import reference_test

# for tt in glob.glob('test/*_test.py'):
#     m = importlib.import_module(tt[:-3].replace('\\', '.'))

def reel_test():
    reference_test.run()

if __name__ == "__main__":
    reel_test()