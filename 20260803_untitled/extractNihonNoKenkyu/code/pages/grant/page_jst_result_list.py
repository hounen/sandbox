from selenium.webdriver.common.by import By

from pages.grant.page_result_base import PageResultBase

class PageJstResultList(PageResultBase):
    def __init__(self, driver):
        super().__init__(driver)

        self.sleep_level = 0

        while True:
            try:
                self.project_list_container = self.find_one(By.XPATH, '//*[@id="result-list"]/div/div[2]/ul', 0)
                self.project_list = self.project_list_container.find_elements(By.XPATH, './li')

                self.project_data_list = []
                for project in self.project_list:
                    project_data = {}

                    project_data['link'] = project.find_element(By.CLASS_NAME, 'link-page').get_attribute("href")
                    project_data['kenkyu_kadai_name'] = project.find_element(By.CLASS_NAME, 'link-page').text.strip()

                    summary_table_dict = {}
                    summary_table = project.find_element(By.TAG_NAME, 'table')
                    for row in summary_table.find_elements(By.TAG_NAME, 'tr'):
                        key = row.find_element(By.TAG_NAME, 'th').text
                        obj = row.find_element(By.TAG_NAME, 'td')
                        summary_table_dict[key] = obj


                    # 研究機関
                    kenkyu_kikan_keys = [ ...
                        , '中核機関'
                    ]

                    match_keys = [key for key in kenkyu_kikan_keys if key in summary_table_dict]
                    project_data['kenkyu_kikan_list'] = []
                    if len(match_keys):
                        for kenkyu_kikan_key in match_keys:
                            kenkyu_kikan_names = summary_table_dict[kenkyu_kikan_key].find_elements(By.TAG_NAME, 'span')
                            if len(kenkyu_kikan_names):
                                for kenkyu_kikan_name in kenkyu_kikan_names:
                                    project_data['kenkyu_kikan_list'].append(kenkyu_kikan_name.text.strip())

                    # 研究代表者
                    kenkyu_daihyosha_keys = [ ...
                        , '企業責任者'
                        , '研究責任者'
                        , '研究代表者'
                        , 'プログラムオフィサー'
                        , 'プログラムディレクター'
                        , '運営統括'
                        , '領域アドバイザー'
                        , '研究主幹'
                        , '主たる共同研究者'
                        , '研究分担者/共同研究者'
                        , '研究総括'
                        , '副研究総括'
                    ]

                    match_keys = [key for key in kenkyu_daihyosha_keys if key in summary_table_dict]
                    project_data['kenkyu_daihyosha_list'] = []
                    if len(match_keys):
                        for kenkyu_daihyosha_key in match_keys:
                            kenkyu_daihyosha = {}

                            kenkyu_daihyosha_names = summary_table_dict[kenkyu_daihyosha_key].find_elements(By.TAG_NAME, 'span')
                            kenkyu_daihyosha_syozokukikan_busyo_yakusyoku = summary_table_dict[kenkyu_daihyosha_key].text
                            for kenkyu_daihyosha_name in kenkyu_daihyosha_names:
                                kenkyu_daihyosha_syozokukikan_busyo_yakusyoku = kenkyu_daihyosha_syozokukikan_busyo_yakusyoku.replace(kenkyu_daihyosha_name.text, "/")

                            kenkyu_daihyosha_syozokukikan_busyo_yakusyoku_list = kenkyu_daihyosha_syozokukikan_busyo_yakusyoku.split("/")
                            for kenkyu_daihyosha_name, kenkyu_daihyosha_syozokukikan_busyo_yakusyoku in zip(kenkyu_daihyosha_names, kenkyu_daihyosha_syozokukikan_busyo_yakusyoku_list[1:]):
                                kenkyu_daihyosha['name'] = kenkyu_daihyosha_name.text.strip()
                                kenkyu_daihyosha_info = kenkyu_daihyosha_syozokukikan_busyo_yakusyoku.split(",")
                                kenkyu_daihyosha['syozokukikan_name'] = kenkyu_daihyosha_info[0].strip()
                                kenkyu_daihyosha['busyo_yakusyoku'] = ",".join(kenkyu_daihyosha_info[1:]) if len(kenkyu_daihyosha_info) > 1 else ""

                            project_data['kenkyu_daihyosha_list'].append(kenkyu_daihyosha)

                    # 研究期間(年度)
                    if '研究期間 (年度)' in summary_table_dict:
                        kenkyu_kikan = summary_table_dict['研究期間 (年度)'].text.replace(" (予定)", "").split(" – ") # 全角ハイフン
                        if len(kenkyu_kikan) == 2:
                            project_data['kenkyu_start_date'] = kenkyu_kikan[0]
                            project_data['kenkyu_end_date'] = kenkyu_kikan[1]
                        elif len(kenkyu_kikan) == 1:
                            project_data['kenkyu_start_date'] = kenkyu_kikan[0]
                            project_data['kenkyu_end_date'] = kenkyu_kikan[0]
                        else:
                            self.logger.error(f"irregular type data : {self.driver.current_url}")
                    else:
                        # self.logger.error(f"研究期間が登録されていません : {project_data['link']}")
                        project_data['kenkyu_start_date'] = ""
                        project_data['kenkyu_end_date'] = ""

                    self.project_data_list.append(project_data)

                break

            except Exception as e:
                self.sleep_level += 1
                if self.sleep_level > int(self.config['OPTION']['PAGE_RETRY']):
                    raise
                self.sleep(float(self.config['OPTION']['SLEEP_TIME'])*self.sleep_level)

    def get_kenkyu_kikan_list(self):
        ret = []

        self.sleep_level = 0

        for project_data in self.project_data_list:
            ret.extend(project_data['kenkyu_kikan_list'])
            ret.extend([kenkyu_daihyosha['syozokukikan_name']for kenkyu_daihyosha in project_data['kenkyu_daihyosha_list']])
        
        return ret

    def get_project_url_list(self):
        ret = []

        for project in self.project_list:
            project_url = project.find_element(By.CLASS_NAME, 'link-page').get_attribute("href")
            ret.append(project_url)

        return ret
