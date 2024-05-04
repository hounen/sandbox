require "yaml"
require "mail"
require "clockwork"
require "selenium-webdriver"
require "time"

include Clockwork

LISTENING_YELL = 60
INPUT_INTERVAL = 5

@driver = Selenium::WebDriver.for :chrome # :edge
@driver.navigate.to "https://shovelful.vercel.app/speech/"

loop do

  request_yell = @driver.find_element(xpath: '//*[@id="root"]/div[1]/div[1]/div/input')
  sleep INPUT_INTERVAL
  request_yell.send_keys('わたしはいまべんきょうしています。はげましてください。')
  sleep INPUT_INTERVAL
  @driver.switch_to.active_element.send_keys(:return)
  sleep LISTENING_YELL
end

@driver.quit
