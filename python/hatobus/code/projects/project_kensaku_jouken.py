from datetime import datetime

class ProjectKensakuJouken:
    def __init__(self, config):
        self.kenkyu_kikan_code = None
        self.kenkyu_kikan_name = None
        self.kenkyu_kikan_from = config['CROSS_SEARCH']['KENKYU_KIKAN_FROM']
        self.kenkyu_yosan_from = int(config['CROSS_SEARCH']['KENKYU_YOSAN_FROM'])*10000