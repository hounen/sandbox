from datetime import datetime
import json

from selenium.webdriver.common.by import By

from pages.page_security import PageSecurity

class PageProjectSummary(PageSecurity):
    def __init__(self, driver):
        super().__init__(driver)
        
        # TODO 要素が存在しないパターン場合の考慮(find_oneでかけるところ)
        # 研究課題
        self.lbl_project_title = self.find_one(By.CLASS_NAME, 'page-title', 0).find_element(By.TAG_NAME, 'h1').text.strip()
        self.tbl_project_summary = self.find_one(By.TAG_NAME, 'table', 0)
        self.lbl_kenkyu_kadai_id = self.tbl_project_summary.find_element(By.XPATH, "//tr[th[normalize-space()='研究課題/領域番号']]/td").text.strip()
        # 研究期間
        self.lbl_kenkyu_kikan = self.tbl_project_summary.find_element(By.XPATH, "//tr[th[normalize-space()='研究期間 (年度)']]/td").text.strip()
        self.lst_kenkyu_kikan = self.lbl_kenkyu_kikan.split()
        self.tim_kenkyu_kikan_from = datetime.strptime(self.lst_kenkyu_kikan[0], "%Y-%m-%d").date()
        self.tim_kenkyu_kikan_to = datetime.strptime(self.lst_kenkyu_kikan[-1], "%Y-%m-%d").date()
        # 研究代表者
        self.row_kenkyu_daihyosha = self.tbl_project_summary.find_element(By.XPATH, "//tr[th[normalize-space()='研究代表者']]/td")
        self.lbl_kenkyu_daihyosha_name = self.row_kenkyu_daihyosha.find_element(By.XPATH, "./h4/span").text.strip()
        self.lbl_kenkyu_daihyosha_syozoku_name = self.row_kenkyu_daihyosha.text.replace(self.lbl_kenkyu_daihyosha_name, "").strip()
        self.lst_kenkyu_daihyosha_syozoku_name = self.lbl_kenkyu_daihyosha_syozoku_name.split()
        self.lbl_kenkyu_daihyosha_syozoku_kikan_name = self.lst_kenkyu_daihyosha_syozoku_name[0].strip(" ,")
        self.lbl_kenkyu_daihyosha_syozoku_busyo_name =  self.lst_kenkyu_daihyosha_syozoku_name[1].strip(" ,")
        self.lbl_kenkyu_daihyosha_syozoku_yakusyoku_name = self.lst_kenkyu_daihyosha_syozoku_name[2].strip(" ,")
        self.lbl_kenkyu_daihyosha_syozoku_yakusyoku_code = self.lst_kenkyu_daihyosha_syozoku_name[-1].strip("()")
        # 配分額
        self.row_haibun_gaku = self.tbl_project_summary.find_element(By.XPATH, "//tr[th[contains(normalize-space(), '配分額')]]/td")
        # 配分額(合計)
        self.lbl_haibun_gaku_total = self.row_haibun_gaku.find_element(By.XPATH, "./h5").text.strip()
        self.dct_haibun_gaku_total = self.extract_total_keihi_info(self.lbl_haibun_gaku_total)
        self.txt_haibun_gaku_total_sougou = self.dct_haibun_gaku_total['合計経費']
        self.txt_haibun_gaku_total_tyokusetsu = self.dct_haibun_gaku_total['直接経費'] 
        self.txt_haibun_gaku_total_kansetsu = self.dct_haibun_gaku_total['間接経費']
        # 配分額(年別)
        self.lst_haibun_gaku_years = self.row_haibun_gaku.text.replace(self.lbl_haibun_gaku_total, "").split("\n")[1:]
        self.dct_haibun_gaku_years = self.extract_nendo_keihi_info(self.lst_haibun_gaku_years)
        # キーワード
        self.row_keyword = self.tbl_project_summary.find_element(By.XPATH, "//tr[th[normalize-space()='キーワード']]/td").text.strip()
        # 研究概要
        self.row_project_gaiyou = self.tbl_project_summary.find_element(By.XPATH, "//tr[th[normalize-space()='研究開始時の研究の概要']]/td").text.strip()
        # 参照URL
        self.url_project_summary = self.driver.current_url
    
    def extract_total_keihi_info(self, str_total_keihi):
        ret = {}
        splited_str_total_keihi = str_total_keihi.replace(",", "").split("千円")

        ret['合計経費'] = int(splited_str_total_keihi[0])
        ret['直接経費'] = int(splited_str_total_keihi[1].split()[-1])
        ret['間接経費'] = int(splited_str_total_keihi[2].split()[-1])

        return ret

    def extract_nendo_keihi_info(self, lst_nendo_keihi):
        ret = []
        if len(lst_nendo_keihi):
            for str_nendo_keihi in lst_nendo_keihi:
                keihi = {}
                splited_str_nendo_keihi = str_nendo_keihi.replace(",", "").split("年度: ")
                keihi['年度'] = int(splited_str_nendo_keihi[0])
                resplited_str_nendo_keihi = splited_str_nendo_keihi[1].split("千円")
                keihi['合計経費'] = int(resplited_str_nendo_keihi[0])
                keihi['直接経費'] = int(resplited_str_nendo_keihi[1].split()[-1])
                keihi['間接経費'] = int(resplited_str_nendo_keihi[2].split()[-1])
                ret.append(keihi)

        return ret
