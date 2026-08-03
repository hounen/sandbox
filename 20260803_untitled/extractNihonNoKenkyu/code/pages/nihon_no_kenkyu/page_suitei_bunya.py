from selenium.webdriver.common.by import By

from pages.page_security import PageSecurity

class PageSuiteiBunya(PageSecurity):
    def __init__(self, driver):
        super().__init__(driver)
        self.sleep()
        self.suitei_bunya_table = self.find_one(By.XPATH, '/html/body/section[1]/section/section/article/div[1]/div/div[6]/div[1]/div/table/tbody', 0)
    
    def to_list(self):
        ret = []
        rows = self.suitei_bunya_table.find_elements(By.TAG_NAME, 'tr')
        for row in rows:
            cols = row.find_elements(By.TAG_NAME, 'td')

            if len(cols):
                name = cols[0].text.strip()
                ret.append(f"{name}")
            else:
                # TODO
                print('irregular type data')

        return ret
    