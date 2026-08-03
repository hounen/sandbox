from selenium.webdriver.common.by import By

from pages.grant.page_result_base import PageResultBase

class PageAmedResultList(PageResultBase):
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

                    # 研究代表者
                    if '研究代表者' in summary_table_dict:
                        kenkyu_daihyosha_name = summary_table_dict['研究代表者'].find_elements(By.TAG_NAME, 'span')[0].text
                        kenkyu_daihyosha_info = summary_table_dict['研究代表者'].text.replace(kenkyu_daihyosha_name, "").split(",") # find_elements(By.TAG_NAME, 'span')
                        if len(kenkyu_daihyosha_info):
                            project_data['kenkyu_daihyosha_name'] = kenkyu_daihyosha_name
                            project_data['kenkyu_daihyosha_syozoku_kikan_name'] = kenkyu_daihyosha_info[0].strip()
                            if len(kenkyu_daihyosha_info) > 1:
                                project_data['kenkyu_daihyosha_syozoku_busyo_yakusyoku'] = kenkyu_daihyosha_info[1].strip()
                            else:
                                project_data['kenkyu_daihyosha_syozoku_busyo_yakusyoku'] = ""
                        else:
                            self.logger.error(f"irregular type data : {self.driver.current_url}")
                    else:
                        self.logger.error(f"irregular type data : {self.driver.current_url}")

                    # 研究期間(年度)
                    if '研究期間 (年度)' in summary_table_dict:
                        kenkyu_kikan = summary_table_dict['研究期間 (年度)'].text.split(" – ") # 全角ハイフン
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

    def get_kenkyu_daihyosha_syozoku_kikan_list(self):
        ret = []

        self.sleep_level = 0

        while True:
            try:
                for project in self.project_list:
                    lbl_kenkyu_daihyosha = project.find_element(By.XPATH, "./div/table/tbody/tr[th[normalize-space()='研究代表者']]/td")
                    lbl_kenkyu_daihyosha_name = lbl_kenkyu_daihyosha.find_element(By.XPATH, "./h4/span").text.strip()
                    lbl_kenkyu_daihyosha_syozoku_name = lbl_kenkyu_daihyosha.text.replace(lbl_kenkyu_daihyosha_name, "").strip()
                    lbl_kenkyu_daihyosha_syozoku_kikan_name = lbl_kenkyu_daihyosha_syozoku_name.split(",")[0].strip()
                    ret.append(lbl_kenkyu_daihyosha_syozoku_kikan_name)

                break

            except Exception as e:
                self.sleep_level += 1
                if self.sleep_level > int(self.config['OPTION']['PAGE_RETRY']):
                    raise
                self.sleep(float(self.config['OPTION']['SLEEP_TIME'])*self.sleep_level)
        
        return ret

    def get_project_url_list(self):
        ret = []

        for project in self.project_list:
            project_url = project.find_element(By.CLASS_NAME, 'link-page').get_attribute("href")
            ret.append(project_url)

        return ret
