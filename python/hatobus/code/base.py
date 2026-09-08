import sys
import configparser
from time import sleep

import logging
from logging.handlers import RotatingFileHandler
# from logging.handlers import StreamHandler

class Base:

    logger = logging.getLogger("extractNihonNoKenkyu")
    config = configparser.ConfigParser()

    def __init__(self):
        ...

    @classmethod
    def logger_settings(cls):
        cls.logger.setLevel(logging.INFO) # ERROR

        formatter = logging.Formatter("%(asctime)s - %(levelname)s - %(message)s")
        stream_handler = logging.StreamHandler(sys.stdout)
        rotating_handler = RotatingFileHandler(
            "./log/log.txt", 
            maxBytes=10*1024*1024, 
            backupCount=5, 
            encoding="utf-8"
        )

        stream_handler.setFormatter(formatter)
        rotating_handler.setFormatter(formatter)

        cls.logger.addHandler(stream_handler)
        cls.logger.addHandler(rotating_handler)

    def sleep(self, sleep_time=None):
        if sleep_time:
            sleep(sleep_time)
        else:
            sleep(float(self.config['OPTION']['SLEEP_TIME']))
