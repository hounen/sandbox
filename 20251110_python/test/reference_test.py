import importlib
import glob

from test.form.main import Main
from test.form.resistance import Resistance

# for tf in glob.glob('test/forms/**/*.py'):
#     importlib.import_module(tf[:-3].replace('\\', '.'))

def run():
    main = Main()
    resistance = Resistance()
    print("Reference test executed.")
