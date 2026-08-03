class ProjectBase:
    def __init__(self):
        self.jigyou_kubun_name = None
        self.kenkyu_kadai_url = None
        self.kenkyu_kadai_name = None
        self.kenkyu_start_date = None
        self.kenkyu_end_date = None
        self.kenkyu_kikan_name = None
        self.kenkyu_daihyosha_name = None
        self.kenkyu_daihyosha_syozoku_kikan_name = None
        self.kenkyu_daihyosha_syozoku_busyo_name = None
        self.kenkyu_daihyosha_yakusyoku_name = None
        self.kenkyu_haibungaku_total = 0
        self.kenkyu_haibungaku_total_direct = 0
        self.kenkyu_haibungaku_total_indirect = 0

        # self.jigyou_kubun_code = None
        # self.kenkyu_kikan_code = None
        # self.kenkyu_daihyosha_yakusyoku_code = None
        # self.kenkyu_gaiyou = None

    def to_dict(self):
        # return vars(self)
        return self.__dict__.copy()
