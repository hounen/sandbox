from time import sleep

from selenium.webdriver.common.by import By

from pages.page_security import PageSecurity

class PageJigyouKubunGroup(PageSecurity):
    def __init__(self, driver):
        super().__init__(driver)

        self.sleep()
        self.jigyou_kubun_group = self.find_one(By.XPATH, '/html/body/section[1]/section/section/article/div[1]/div/div[3]', 0)
    
    def to_list(self):
        ret = []
        rows = self.jigyou_kubun_group.find_elements(By.XPATH, "./a[@class='list-group-item hidden-xs']")
        for row in rows:
            link = row.get_attribute("href")
            code = link.split('/')[-1].zfill(5)
            name = row.text.strip()
            ret.append(f"{code}: {name}")
        return ret    