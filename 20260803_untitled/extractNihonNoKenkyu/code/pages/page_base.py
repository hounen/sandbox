from time import sleep

from base import Base

class PageBase(Base):
    def __init__(self, driver):
        super().__init__()
        self.driver = driver

    # def __enter__(self):
    #     return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        sleep(self.config['OPTION']['SLEEP_TIME'])
        self.driver.close()
        self.driver.quit()
    
    def find_one(self, locator, selector, index):
        objs = self.driver.find_elements(locator, selector)
        if len(objs) >= 1:
            obj = objs[index]
        else:
            obj = None
        return obj

    def find_elems(self, locator, selector, index):
        objs = self.driver.find_elements(locator, selector)
        if len(objs) >= 1:
            obj = objs[index]
        else:
            obj = None
            print('')
        return obj

    # mikansei
    def navigate_to(self, page_to, action):

        page_next = None

        PageTo = globals().get(page_to)
        if PageTo:
            page_next = PageTo(self.driver)
        else:
            # TODO mitukaranaibaai exception
            page_next = None
        
        return page_next