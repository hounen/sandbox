from selenium.webdriver.common.by import By

from pages.page_base import PageBase

class PageSecurity(PageBase):
    def __init__(self, driver):
        super().__init__(driver)

        if "Internet Security by Zscaler" in self.driver.title:
            print("Skip Zscaler")
            self.btn_zokkousuru = self.find_one(By.XPATH, '//*[@id="ja_JP"]/tbody/tr[8]/td/table/tbody/tr/td/div/form/input[1]', 0)
            self.zokkousuru()
        else:
            ...
            # print("Not Zscaler")

    def zokkousuru(self):
        self.btn_zokkousuru.click()