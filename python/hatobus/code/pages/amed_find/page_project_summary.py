from selenium.webdriver.common.by import By

from pages.page_security import PageSecurity

class PageProjectSummary(PageSecurity):

    def __init__(self, driver):
        super().__init__(driver)

        self.sleep_level = 0

        while True:
            try:

                self.result_list_container = None
                self.theme_basic_info = None
                self.theme_basic_info_data = None
                self.kadai_kanri_no = None
                self.kenkyu_kikan_name = ""
                self.kenkyu_haibungaku_total = 0
                self.award_amounts = []

                # AMED側がリンク切れを起こしている場合
                self.is_not_found = False
                self.is_not_found_text = self.find_one(By.XPATH, '//*[@id="__nuxt"]/div/div/div/main/div/div[1]/div/div', 0).text.strip()
                if self.is_not_found_text == "ご指定の課題が存在しません":
                    self.is_not_found = True
                    break

                self.get_info()

                break

            except Exception as e:
                self.sleep_level += 1
                if self.sleep_level > int(self.config['OPTION']['PAGE_RETRY']):
                    break
                self.sleep(float(self.config['OPTION']['SLEEP_TIME'])*self.sleep_level)

    def get_info(self):
        self.result_list_container = self.find_one(By.XPATH, '//*[@id="result-list"]/div/div[2]/ul', 0)
        self.theme_basic_info = self.find_one(By.XPATH, '//*[@id="theme-basic-info full-width"]', 0)

        self.theme_basic_info_data = {}
        for row in self.theme_basic_info.find_elements(By.XPATH, '//dl/div'):
            key = row.find_element(By.TAG_NAME, 'dt').text
            dds = row.find_elements(By.TAG_NAME, 'dd')
            if len(dds):
                self.theme_basic_info_data[key] = dds[0].text.strip()
                match key:
                    case '課題への総配分額':
                        match len(dds):
                            case 2:
                                self.theme_basic_info_data['年度別の配分額'] = dds[1]
                            case _:
                                ...
                    case _:
                        ...

        if '課題管理番号' in self.theme_basic_info_data:
            self.kadai_kanri_no = self.theme_basic_info_data['課題管理番号']
        if '代表研究機関' in self.theme_basic_info_data:
            self.kenkyu_kikan_name = self.theme_basic_info_data['代表研究機関'] 
        if '課題への総配分額' in self.theme_basic_info_data:
            self.kenkyu_haibungaku_total = self.theme_basic_info_data['課題への総配分額'].replace(" 千円", "").replace(",", "")
        if '年度別の配分額' in self.theme_basic_info_data:
            self.award_amounts = []

            thead = self.theme_basic_info_data['年度別の配分額'].find_elements(By.XPATH, './div/div/table/thead')[0]
            tbody = self.theme_basic_info_data['年度別の配分額'].find_elements(By.XPATH, './div/div/table/tbody')[1]

            ths = thead.find_elements(By.XPATH, './tr/th')
            for tr in tbody.find_elements(By.XPATH, './tr'):
                award_amount = {} 
                for index, th in enumerate(ths, start=0):
                    tds = tr.find_elements(By.XPATH, './td')
                    if len(ths) == len(tds):
                        award_amount[th.text] = tds[index].text
                    else:
                        if not index:
                            award_amount[th.text] = tds[index].text
                        else:
                            award_amount[th.text] = "未確定"
                
                self.award_amounts.append(award_amount)
