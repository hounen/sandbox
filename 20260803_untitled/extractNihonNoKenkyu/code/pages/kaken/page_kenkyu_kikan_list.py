from selenium.webdriver.common.by import By

from pages.page_security import PageSecurity

class PageKenkyuKikanList(PageSecurity):
    def __init__(self, driver):
        super().__init__(driver)
        self.tbl_kenkyu_kikan_list = self.find_one(By.TAG_NAME, 'table', 0)
    
    def to_list(self):
        ret = []
        rows = self.tbl_kenkyu_kikan_list.find_elements(By.TAG_NAME, 'tr')
        for row in rows:
            cols = row.find_elements(By.TAG_NAME, 'td')

            if len(cols):
                code = cols[1].text.strip()
                name = cols[0].text.strip()
                # ret.append(f"{code}: {name}")
                ret.append(f"{name}")
            else:
                print('irregular type data')

        return ret