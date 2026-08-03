from selenium.webdriver.common.by import By

from pages.page_security import PageSecurity

class PageResultBase(PageSecurity):
    def __init__(self, driver):
        super().__init__(driver)

        self.sleep_level = 0

        while True:
            try:
                self.result_list = self.find_one(By.XPATH, '//*[@id="result-list"]', 0)
                self.pagingitem_menu = self.find_one(By.XPATH, '//*[@id="result-list"]/div/div[1]/div[1]/div/ul' ,0)
                self.total_page_count = self.get_total_page_count()

                break
            except Exception as e:
                self.sleep_level += 1
                if self.sleep_level > int(self.config['OPTION']['PAGE_RETRY']):
                    raise
                self.sleep(float(self.config['OPTION']['SLEEP_TIME'])*self.sleep_level)

    def get_total_page_count(self):
        if self.result_list is None:
            # 0件
            ret = 0
        else:
            if self.pagingitem_menu is None:
                # 1ページ
                ret = 1
            else:
                # 2ページ以上
                self.pagingitems = self.pagingitem_menu.find_elements(By.TAG_NAME, 'a')
                ret = int(self.pagingitems[-1].get_attribute("id").split("_")[-1])

        return ret
             

        
        


    
