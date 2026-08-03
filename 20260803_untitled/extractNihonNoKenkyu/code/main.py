from time import sleep
import yaml
import sys
import os
import pandas as pd
from datetime import datetime
import requests
import xmltodict
import json
from styleframe import StyleFrame
from urllib.parse import quote
import re
from collections import defaultdict
from rapidfuzz import process, fuzz
from pathlib import Path
import swifter
from shapely.geometry import Point
import geopandas as gpd

from selenium import webdriver
from selenium.webdriver.edge.options import Options
from selenium.webdriver.support.ui import WebDriverWait

from kaken_api import KakenApiClient
from jpcorpreg import CorporateRegistryClient
import unicodedata

from base import Base

from pages.nihon_no_kenkyu.page_jigyou_kubun_group import PageJigyouKubunGroup
from pages.nihon_no_kenkyu.page_suitei_bunya import PageSuiteiBunya

from pages.kaken.page_kenkyu_kikan_list import PageKenkyuKikanList
from pages.kaken.page_project_summary import PageProjectSummary as PageKakenProjectSummary

from pages.grant.page_result_base import PageResultBase
from pages.grant.page_amed_result_list import PageAmedResultList
from pages.grant.page_jst_result_list import PageJstResultList

from pages.amed_find.page_project_summary import PageProjectSummary as PageAmedProjectSummary

from projects.kaken.project_kaken import ProjectKaken
from projects.amed.project_amed import ProjectAmed
from projects.jst.project_jst import ProjectJst
from projects.project_kensaku_jouken import ProjectKensakuJouken


class Main(Base):
    def __init__(self):
        super().__init__()

        Base.logger_settings()
        Base.config.read('./settings/input.ini')

        self.run_time = datetime.now().strftime('%Y%m%d%H%M')
        self.prefecture = gpd.read_file('./database/N03-20260101_GML/N03-20260101.geojson')

        self.options = Options()
        # TODO options no file ka
        self.options.add_argument("--headless")
        self.options.add_argument("--disable-gpu")
        self.options.add_argument("--start-maximized")
        self.options.add_argument("--log-level=3")

        self.driver = webdriver.Edge(options=self.options)
        self.corporate_resistry_client = CorporateRegistryClient()
        self.df_corporate_resistry = pd.DataFrame()
        self.unified_kenkyu_kikan_list = []

        # 出力先
        self.output_base = os.path.join(self.config['OPTION']['OUTPUT_PLACE_PATH'], f"nihon_no_kenkyu_{self.run_time}")

    def __enter__(self):
        self.start_time = datetime.now()
        self.logger.info("準備中...")
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        elapsed_time = datetime.now() - self.start_time
        elapsed_minutes = elapsed_time.total_seconds() / 60
        self.logger.info(f"処理が終了しました (実行時間: {elapsed_minutes:.2f}分)")

    def run(self):

        ### phase 1 検索用研究機関マスタ更新
        if self.config['KAKEN_SWITCH']['KENKYU_KIKAN_SYNC'].lower() == "true":
            target_kaken_kenkyu_kikan = self.update_kaken_kenkyu_kikan_file()
        if self.config['AMED_SWITCH']['KENKYU_KIKAN_SYNC'].lower() == "true":
            target_amed_kenkyu_kikan = self.update_amed_kenkyu_kikan_file()
        if self.config['JST_SWITCH']['KENKYU_KIKAN_SYNC'].lower() == "true":
            target_jst_kenkyu_kikan = self.update_jst_kenkyu_kikan_file()

        ### phase 2 事業別に研究プロジェクトを検索
        print("抽出対象の研究機関を個別に指定する場合は、ここで停止し、検索項目を編集して再実行してください")
        while True:
            answer = input(f"このまま抽出処理を開始しますか? (y/n): ")
            if answer.lower() == "y":
                break
            elif answer.lower() == "n":
                exit()
            else:
                print("y または n を入力してください")

        self.project_kensaku_jouken = ProjectKensakuJouken(self.config)

        # KAKEN(科学研究費助成事業)
        if self.config['KAKEN_SWITCH']['EXTRACTION'].lower() == "true":
            self.extract_kaken_projects()

        # AMED(日本医療研究開発機構)
        if self.config['AMED_SWITCH']['EXTRACTION'].lower() == "true":
            self.extract_amed_projects()

        # JST(科学技術振興機構)
        if self.config['JST_SWITCH']['EXTRACTION'].lower() == "true":
            self.extract_jst_projects()

    def update_kaken_kenkyu_kikan_file(self):
        # 機関別
        while True:
            answer = input(f"研究機関リスト(KAKEN){self.config['KAKEN_SEARCH']['KENKYU_KIKAN_FILE']}を更新(*上書き)しますか? (y/n): ")
            if answer.lower() == "y":
                print("更新中...")
                self.driver.get(self.config['KAKEN_SOURCE']['KENKYU_KIKAN_URL'])
                page_kenkyu_kikan_list = PageKenkyuKikanList(self.driver)
                kenkyu_kikan_list = page_kenkyu_kikan_list.to_list()[1:]
                with open(self.config['KAKEN_SEARCH']['KENKYU_KIKAN_FILE'], 'w', encoding='utf-8') as f:
                    yaml.safe_dump(kenkyu_kikan_list, f, allow_unicode=True, sort_keys=False)
                print("研究機関リスト(KAKEN)を再作成しました")
                break
            elif answer.lower() == "n":
                break
            else:
                print("y または n を入力してください")

    def update_amed_kenkyu_kikan_file(self):
        # 機関別
        while True:
            answer = input(f"研究機関リスト(AMED){self.config['AMED_SEARCH']['KENKYU_KIKAN_FILE']}を更新(*上書き)しますか? (y/n): ")
            if answer.lower() == "y":
                print("更新中...")
                
                amed_kenkyu_kikan_list = []

                # パラメータを作成する
                url_params = "c8%5B%5D=" + "amed"
                url_params += "&" + "rw=" + str(self.config['AMED_OPTION']['RESULTS_PER_PAGE'])
                
                # プロジェクト数を取得する
                self.driver.get(f"{self.config['AMED_SOURCE']['URL']}?{url_params}")
                # ページ数を計算する
                page_result_base = PageResultBase(self.driver)
                # 全ページから研究機関を取得する
                for page_no in range(1, page_result_base.total_page_count+1, int(self.config['AMED_OPTION']['RESULTS_PER_PAGE'])):
                    url_params_with_page = url_params + "&" + "st=" + str(page_no)
                    self.driver.get(f"{self.config['AMED_SOURCE']['URL']}?{url_params_with_page}")
                    # 研究機関リストを取得する
                    page_amed_result_list = PageAmedResultList(self.driver)
                    amed_kenkyu_kikan_list.extend(page_amed_result_list.get_kenkyu_daihyosha_syozoku_kikan_list())
                    self.logger.info(f"{100*page_no/page_result_base.total_page_count:6.2f}% completed. ({page_no:5}projects/{page_result_base.total_page_count})")

                amed_kenkyu_kikan_list = sorted(list(set(amed_kenkyu_kikan_list)))
                
                # 検索用の研究機関リストを作成する
                with open(self.config['AMED_SEARCH']['KENKYU_KIKAN_FILE'], 'w', encoding='utf-8') as f:
                    yaml.safe_dump(amed_kenkyu_kikan_list, f, allow_unicode=True, sort_keys=False)
                print("研究機関リスト(AMED)を再作成しました")
                break
            elif answer.lower() == "n":
                break
            else:
                print("y または n を入力してください")

    def update_jst_kenkyu_kikan_file(self):
        # 機関別
        while True:
            answer = input(f"研究機関リスト(JST){self.config['JST_SEARCH']['KENKYU_KIKAN_FILE']}を更新(*上書き)しますか? (y/n): ")
            if answer.lower() == "y":
                print("更新中...")

                jst_kenkyu_kikan_list = []

                # パラメータを作成する
                url_params = "c8%5B%5D=" + "jst"
                url_params += "&" + "rw=" + str(self.config['JST_OPTION']['RESULTS_PER_PAGE'])

                # プロジェクト数を取得する
                self.driver.get(f"{self.config['JST_SOURCE']['URL']}?{url_params}")
                # ページ数を計算する
                page_result_base = PageResultBase(self.driver)
                # 全ページから研究機関を取得する
                for page_no in range(1, page_result_base.total_page_count+1, int(self.config['JST_OPTION']['RESULTS_PER_PAGE'])):
                    url_params_with_page = url_params + "&" + "st=" + str(page_no)
                    self.driver.get(f"{self.config['JST_SOURCE']['URL']}?{url_params_with_page}")
                    # 研究機関リストを取得する
                    page_jst_result_list = PageJstResultList(self.driver)
                    jst_kenkyu_kikan_list.extend(page_jst_result_list.get_kenkyu_kikan_list())
                    self.logger.info(f"{100*page_no/page_result_base.total_page_count:6.2f}% completed. ({page_no*int(self.config['JST_OPTION']['RESULTS_PER_PAGE']):5}projects/{page_result_base.total_page_count})")

                jst_kenkyu_kikan_list = sorted(list(set(jst_kenkyu_kikan_list)))
                jst_kenkyu_kikan_list = [kenkyu_kikan_name for kenkyu_kikan_name in jst_kenkyu_kikan_list if kenkyu_kikan_name != '']

                # 検索用の研究機関リストを作成する
                with open(self.config['JST_SEARCH']['KENKYU_KIKAN_FILE'], 'w', encoding='utf-8') as f:
                    yaml.safe_dump(jst_kenkyu_kikan_list, f, allow_unicode=True, sort_keys=False)
                print("研究機関リスト(JST)を再作成しました")
                break
            elif answer.lower() == "n":
                break
            else:
                print("y または n を入力してください")

    def extract_kaken_projects(self):
        # KAKEN(科学研究費助成事業)
        with open(self.config['KAKEN_SEARCH']['KENKYU_KIKAN_FILE'], encoding='utf-8') as file:
            target_kaken_kenkyu_kikan = yaml.safe_load(file)
        
        for index, kenkyu_kikan in enumerate(target_kaken_kenkyu_kikan, start=1):
            output_projects = []
            # self.project_kensaku_jouken.kenkyu_kikan_code = kenkyu_kikan.split(": ")[0]
            self.project_kensaku_jouken.kenkyu_kikan_name = kenkyu_kikan # .split(": ")[1]
            output_projects = self.request_kaken_projects()
            if len(output_projects):
                self.output_projects(output_projects, "kaken", kenkyu_kikan, sheetname="一覧")
        
            self.logger.info(f"{100*index/len(target_kaken_kenkyu_kikan):6.2f}% completed. ({len(output_projects):5}projects) KAKEN: {self.project_kensaku_jouken.kenkyu_kikan_name}")

        self.listup_kenkyu_kikan("kaken")

    def extract_amed_projects(self):
        # AMED(日本医療研究開発機構)
        with open(self.config['AMED_SEARCH']['KENKYU_KIKAN_FILE'], encoding='utf-8') as file:
            target_amed_kenkyu_kikan = yaml.safe_load(file)

        for index, kenkyu_kikan in enumerate(target_amed_kenkyu_kikan, start=1):
            output_projects = []
            self.project_kensaku_jouken.kenkyu_kikan_name = kenkyu_kikan
            output_projects = self.request_amed_projects()
            if len(output_projects):
                self.output_projects(output_projects, "amed", kenkyu_kikan, sheetname="一覧")

            self.logger.info(f"{100*index/len(target_amed_kenkyu_kikan):6.2f}% completed. ({len(output_projects):5}projects) AMED : {self.project_kensaku_jouken.kenkyu_kikan_name}")

        self.listup_kenkyu_kikan("amed")

    def extract_jst_projects(self):
        # JST(科学技術振興機構)
        with open(self.config['JST_SEARCH']['KENKYU_KIKAN_FILE'], encoding='utf-8') as file:
            target_jst_kenkyu_kikan = yaml.safe_load(file)

        for index, kenkyu_kikan in enumerate(target_jst_kenkyu_kikan, start=1):
            output_projects = []
            self.project_kensaku_jouken.kenkyu_kikan_name = kenkyu_kikan
            output_projects = self.request_jst_projects()
            if len(output_projects):
                self.output_projects(output_projects, "jst", kenkyu_kikan, sheetname="一覧")

            self.logger.info(f"{100*index/len(target_jst_kenkyu_kikan):6.2f}% completed. ({len(output_projects):5}projects) JST : {self.project_kensaku_jouken.kenkyu_kikan_name}")

        self.listup_kenkyu_kikan("jst")

    def request_kaken_projects(self):

        ret_projects = []

        client = KakenApiClient(
            app_id=self.config['CINII_OPENSEARCH']['APPID'],
            use_cache=self.config['CINII_OPENSEARCH']['USE_CACHE'].lower() == "true",
            # cache_dir=self.config['CINII_OPENSEARCH']['CACHE_PATH'],
        )

        while True:
            try:
                res_projects = client.projects.search(
                    institution=self.project_kensaku_jouken.kenkyu_kikan_name,
                    results_per_page=int(self.config['KAKEN_OPTION']['RESULTS_PER_PAGE']),
                    grant_period_from=self.project_kensaku_jouken.kenkyu_kikan_from,
                    grant_period_condition="1",
                    language="ja",
                )
                break
            except Exception as e:
                self.logger.error(f"API通信に失敗しました。リトライします。: {e}")
                continue

        if res_projects is None or not res_projects.total_results:
            ...
        else:
            request_count = res_projects.total_results // res_projects.items_per_page
            if res_projects.total_results % res_projects.items_per_page:
                request_count += 1

            if request_count:

                for request_index in range(request_count):
                    # self.logger.info(f"{request_index + 1}/{request_count}request")

                    while True:
                        try:
                            res_projects = client.projects.search(
                                institution=self.project_kensaku_jouken.kenkyu_kikan_name,
                                start_index=(int(self.config['KAKEN_OPTION']['RESULTS_PER_PAGE']) * request_index + 1),
                                results_per_page=int(self.config['KAKEN_OPTION']['RESULTS_PER_PAGE']),
                                grant_period_from=self.project_kensaku_jouken.kenkyu_kikan_from,
                                grant_period_condition="1",
                                language="ja",
                            )
                            break
                        except Exception as e:
                            self.logger.error(f"API通信に失敗しました。リトライします。: {e}")
                            continue

                    for res_project in res_projects.projects:
                        try:
                            ret_project = ProjectKaken()
                            # # 事業区分コード
                            # ret_project.jigyou_kubun_code = self.project_kensaku_jouken.jigyou_kubun_code
                            # 事業区分名
                            ret_project.jigyou_kubun_name = "(KAKEN)科学研究費助成事業"
                            # # 研究機関コード
                            # ret_project.kenkyu_kikan_code = self.project_kensaku_jouken.kenkyu_kikan_code
                            # (プロジェクトデータ)
                            res_project_grant_award = xmltodict.parse(res_project.raw_data)['grantAward']
                            # URL
                            ret_project.kenkyu_kadai_url = self.get_nested(res_project_grant_award, ['urlList', 'url'])
                            # (サマリデータ)
                            res_project_summary = res_project_grant_award['summary'][0]
                            # 研究課題名
                            if res_project.title is not None:
                                ret_project.kenkyu_kadai_name = res_project.title
                            else:
                                ret_project.kenkyu_kadai_name = res_project_grant_award['summary'][1]['title']
                            # 研究機関名
                            kenkyu_kikan_name = ""
                            if isinstance(res_project_summary['institution'], list):
                                for institution in res_project_summary['institution']:
                                    kenkyu_kikan_name = kenkyu_kikan_name + institution['#text'] + "/"
                            elif isinstance(res_project_summary['institution'], dict):
                                kenkyu_kikan_name = res_project_summary['institution']['#text']
                            else:
                                print('irregular type data')
                            ret_project.kenkyu_kikan_name = kenkyu_kikan_name
                            # 研究期間(開始)
                            if 'startDate' in res_project_summary['periodOfAward']:
                                ret_project.kenkyu_start_date = datetime.strptime(res_project_summary['periodOfAward']['startDate'], "%Y-%m-%d").date()
                            elif '@searchStartFiscalYear' in res_project_summary['periodOfAward']:
                                ret_project.kenkyu_start_date = datetime.strptime(res_project_summary['periodOfAward']['@searchStartFiscalYear'] + "-04-01", "%Y-%m-%d").date()
                            else:
                                ret_project.kenkyu_start_date = None
                            # 研究期間(終了)
                            if 'endDate' in res_project_summary['periodOfAward']:
                                ret_project.kenkyu_end_date = datetime.strptime(res_project_summary['periodOfAward']['endDate']['#text'], "%Y-%m-%d").date()
                            elif '@searchEndFiscalYear' in res_project_summary['periodOfAward']:
                                ret_project.kenkyu_end_date = datetime.strptime(str(int(res_project_summary['periodOfAward']['@searchEndFiscalYear'])+1) + "-03-31", "%Y-%m-%d").date()
                            else:
                                ret_project.kenkyu_end_date = None
                            # 配分額(合計)
                            total_cost = self.get_nested(res_project_summary, ['overallAwardAmount', 'totalCost']) 
                            ret_project.kenkyu_haibungaku_total = int(total_cost) if total_cost is not None else 0
                            direct_cost = self.get_nested(res_project_summary, ['overallAwardAmount', 'directCost'])
                            ret_project.kenkyu_haibungaku_total_direct = int(direct_cost) if direct_cost is not None else 0
                            indirect_cost = self.get_nested(res_project_summary, ['overallAwardAmount', 'indirectCost'])
                            ret_project.kenkyu_haibungaku_total_indirect = int(indirect_cost) if indirect_cost is not None else 0
                            # 配分額(年度)
                            award_amounts = self.get_nested(res_project_grant_award, ['awardAmountList', 'awardAmount'])
                            if award_amounts is not None:
                                if isinstance(award_amounts, list):
                                    for award_amount in award_amounts:
                                        setattr(ret_project, f"{award_amount['@fiscalYear']}_kenkyu_haibungaku_total", int(award_amount['totalCost']) if 'totalCost' in award_amount else 0)
                                        setattr(ret_project, f"{award_amount['@fiscalYear']}_kenkyu_haibungaku_total_direct", int(award_amount['directCost']) if 'directCost' in award_amount else 0)
                                        setattr(ret_project, f"{award_amount['@fiscalYear']}_kenkyu_haibungaku_total_indirect", int(award_amount['indirectCost']) if 'indirectCost' in award_amount else 0)
                                elif isinstance(award_amounts, dict):
                                        setattr(ret_project, f"{award_amounts['@fiscalYear']}_kenkyu_haibungaku_total", int(award_amounts['totalCost']) if 'totalCost' in award_amounts else 0)
                                        setattr(ret_project, f"{award_amounts['@fiscalYear']}_kenkyu_haibungaku_total_direct", int(award_amounts['directCost']) if 'directCost' in award_amounts else 0)
                                        setattr(ret_project, f"{award_amounts['@fiscalYear']}_kenkyu_haibungaku_total_indirect", int(award_amounts['indirectCost']) if 'indirectCost' in award_amounts else 0)
                                else:
                                    print('irregular type data')
                            # (研究代表者)
                            # TODO (熊本大学)研究代表者が複数人のパターンあり
                            kenkyu_daihyosha = None
                            if isinstance(res_project_summary['member'], list):
                                for member in res_project_summary['member']:
                                    if member['@role'] == "principal_investigator":
                                        kenkyu_daihyosha = member
                                        break
                                    else:
                                        continue
                            elif isinstance(res_project_summary['member'], dict):
                                kenkyu_daihyosha = res_project_summary['member']
                            else:
                                print('irregular type data')
                            # 研究代表者名
                            kenkyu_daihyosha_name = ""
                            if isinstance(kenkyu_daihyosha['personalName'], list):
                                for personal_name in kenkyu_daihyosha['personalName']:
                                    kenkyu_daihyosha_name = kenkyu_daihyosha_name + personal_name['fullName'] + "/"
                            elif isinstance(kenkyu_daihyosha['personalName'], dict):
                                kenkyu_daihyosha_name = kenkyu_daihyosha['personalName']['fullName']
                            else:
                                print('irregular type data')
                            ret_project.kenkyu_daihyosha_name = kenkyu_daihyosha_name
                            # 研究代表者所属機関名
                            ret_project.kenkyu_daihyosha_syozoku_kikan_name = self.get_nested(kenkyu_daihyosha, ['affiliation', 'institution'])
                            # 研究代表者所属部署名
                            ret_project.kenkyu_daihyosha_syozoku_busyo_name = self.get_nested(kenkyu_daihyosha, ['affiliation', 'department'])
                            # 研究代表者役職名
                            ret_project.kenkyu_daihyosha_yakusyoku_name = self.get_nested(kenkyu_daihyosha, ['affiliation', 'jobTitle'])

                            # 検索対象外のデータを除外する

                            # 配分額(合計)
                            if int(ret_project.kenkyu_haibungaku_total) < self.project_kensaku_jouken.kenkyu_yosan_from:
                                continue

                            ret_projects.append(ret_project.to_dict())

                        except Exception as e:
                            self.logger.error(f"プロジェクトの抽出に失敗しました: {e}")
                            self.logger.error(f"失敗したプロジェクトのリンク先: {ret_project.kenkyu_kadai_url}")
                            with open('./debug/nihon_no_kenkyu_raw_data.json', 'w') as file:
                                raw_data = xmltodict.parse(res_project.raw_data)
                                json.dump(raw_data, file, indent=2)
                            continue

        return ret_projects

    def request_amed_projects(self):
        ret_projects = []

        # パラメータ作成する
        url_params = "c8%5B%5D=" + "amed"
        url_params += "&" + "rw=" + str(self.config['AMED_OPTION']['RESULTS_PER_PAGE'])
        url_params += "&" + "o1=" + str(self.config['AMED_SEARCH']['KENKYU_KIKAN_CONDITION']) # サーバー側が3(実施期間の一部を含む)は機能してない
        url_params += "&" + "s1=" + str(self.config['CROSS_SEARCH']['KENKYU_KIKAN_FROM'])
        url_params += "&" + "qh=" + quote(self.project_kensaku_jouken.kenkyu_kikan_name)

        self.driver.get(f"{self.config['AMED_SOURCE']['URL']}?{url_params}")
        page_result_base = PageResultBase(self.driver)

        # AMEDサイトへのURLリストを取得する
        if page_result_base.total_page_count:
            for page_no in range(1, page_result_base.total_page_count+1, int(self.config['AMED_OPTION']['RESULTS_PER_PAGE'])):
                try:
                    url_params_with_page = url_params + "&" + "st=" + str(page_no)
                    self.driver.get(f"{self.config['AMED_SOURCE']['URL']}?{url_params_with_page}")
                    page_amed_result_list = PageAmedResultList(self.driver)

                    for project_data in page_amed_result_list.project_data_list:
                        ret_project = ProjectAmed()
                        ret_project.jigyou_kubun_name = "(AMED)日本医療研究開発機構"
                        ret_project.kenkyu_kadai_url = project_data['link']
                        ret_project.kenkyu_kadai_name = project_data['kenkyu_kadai_name']
                        ret_project.kenkyu_kikan_name = project_data['kenkyu_daihyosha_syozoku_kikan_name']

                        # 研究期間(開始)
                        if 'kenkyu_start_date' in project_data:
                            match project_data['kenkyu_start_date']:
                                case str_date if re.fullmatch(r"\d{4}", str_date):
                                    ret_project.kenkyu_start_date = datetime.strptime(project_data['kenkyu_start_date'] + "-04-01", "%Y-%m-%d").date()
                                case str_date if re.fullmatch(r"\d{4}-\d{2}-\d{2}", str_date):
                                    ret_project.kenkyu_start_date = datetime.strptime(project_data['kenkyu_start_date'], "%Y-%m-%d").date()
                                case _:
                                    ret_project.kenkyu_start_date = None
                        else:
                            ret_project.kenkyu_start_date = None

                        # 研究期間(終了)
                        if 'kenkyu_end_date' in project_data:
                            match project_data['kenkyu_end_date']:
                                case str_date if re.fullmatch(r"\d{4}", str_date):
                                    ret_project.kenkyu_end_date = datetime.strptime(str(int(project_data['kenkyu_end_date'])+1) + "-03-31", "%Y-%m-%d").date()
                                case str_date if re.fullmatch(r"\d{4}-\d{2}-\d{2}", str_date):
                                    ret_project.kenkyu_end_date = datetime.strptime(project_data['kenkyu_end_date'], "%Y-%m-%d").date()
                                case _:
                                    ret_project.kenkyu_end_date = None
                        else:
                            ret_project.kenkyu_end_date = None

                        # 研究代表者名
                        ret_project.kenkyu_daihyosha_name =  project_data['kenkyu_daihyosha_name']
                        # 研究代表者所属機関
                        ret_project.kenkyu_daihyosha_syozoku_kikan_name = project_data['kenkyu_daihyosha_syozoku_kikan_name']
                        # 研究代表者所属部署
                        ret_project.kenkyu_daihyosha_syozoku_busyo_name = "(役職に記載)" # project_data['kenkyu_daihyosha_syozoku_busyo_yakusyoku']
                        # 研究代表者役職
                        ret_project.kenkyu_daihyosha_yakusyoku_name = project_data['kenkyu_daihyosha_syozoku_busyo_yakusyoku']

                        try:
                            # AMED_findの画面から代表研究機関と配分額を取得する
                            self.driver.get(project_data['link'])
                            page_amed_project_summary = PageAmedProjectSummary(self.driver)

                            if not page_amed_project_summary.is_not_found:

                                # # 代表研究機関名
                                # if page_amed_project_summary.kenkyu_kikan_name is not None:
                                #     ret_project.kenkyu_kikan_name = page_amed_project_summary.kenkyu_kikan_name

                                # 総配分額
                                ret_project.kenkyu_haibungaku_total = int(page_amed_project_summary.kenkyu_haibungaku_total)*1000
                                ret_project.kenkyu_haibungaku_total_direct = ""
                                ret_project.kenkyu_haibungaku_total_indirect = ""
                                # 年度別配分額
                                if page_amed_project_summary.award_amounts is not None:
                                    for award_amount in page_amed_project_summary.award_amounts:
                                        # 総配分額
                                        if award_amount['総配分額（千円）'] == "未確定":
                                            kenkyu_haibungaku_total = "未確定"
                                        else:
                                            kenkyu_haibungaku_total = int(award_amount['総配分額（千円）'].replace(",", ""))*1000

                                        setattr(ret_project, f"{award_amount['年度']}_kenkyu_haibungaku_total",  kenkyu_haibungaku_total)
                                        setattr(ret_project, f"{award_amount['年度']}_kenkyu_haibungaku_total_direct",  "")
                                        setattr(ret_project, f"{award_amount['年度']}_kenkyu_haibungaku_total_indirect",  "")

                                    # (GRANT側になかった場合の)研究期間(開始)
                                    if ret_project.kenkyu_start_date is None:
                                        match page_amed_project_summary.award_amounts[-1]['年度']:
                                            case str_date if re.fullmatch(r"\d{4}", str_date):
                                                ret_project.kenkyu_start_date = datetime.strptime(str_date + "-04-01", "%Y-%m-%d").date()
                                            case _:
                                                ret_project.kenkyu_start_date = None

                                    # (GRANT側になかった場合の)研究期間(終了)
                                    if ret_project.kenkyu_end_date is None:
                                        match page_amed_project_summary.award_amounts[0]['年度']:
                                            case str_date if re.fullmatch(r"\d{4}", str_date):
                                                ret_project.kenkyu_end_date = datetime.strptime(str(int(str_date)+1) + "-03-31", "%Y-%m-%d").date()
                                            case _:
                                                ret_project.kenkyu_end_date = None

                                    # 検索対象外データを除外する

                                # 配分額(合計)
                                if ret_project.kenkyu_haibungaku_total == 0:
                                    pass # 未確定は出力する
                                else:
                                    if ret_project.kenkyu_haibungaku_total < self.project_kensaku_jouken.kenkyu_yosan_from:
                                        continue

                                ret_projects.append(ret_project.to_dict())

                        except Exception as e:
                            self.logger.error(f"プロジェクトの抽出に失敗しました: {e}")
                            self.logger.error(f"失敗したプロジェクトリストのリンク先: {ret_project.kenkyu_kadai_url}")
                            with open('./debug/amed.json', 'w') as file:
                                json.dump(ret_project, file, indent=2)
                            continue

                except Exception as e:
                    self.logger.error(f"プロジェクトの抽出に失敗しました: {e}")
                    self.logger.error(f"失敗したプロジェクトリストのリンク先: {self.driver.current_url}")
                    continue
        
        return ret_projects

    def request_jst_projects(self):
        ret_projects = []

        # パラメータ作成する
        url_params = "c8%5B%5D=" + "jst"
        url_params += "&" + "rw=" + str(self.config['JST_OPTION']['RESULTS_PER_PAGE'])
        url_params += "&" + "o1=" + str(self.config['JST_SEARCH']['KENKYU_KIKAN_CONDITION']) # サーバー側が3(実施期間の一部を含む)は機能してない
        url_params += "&" + "s1=" + str(self.config['CROSS_SEARCH']['KENKYU_KIKAN_FROM'])
        url_params += "&" + "qh=" + quote(self.project_kensaku_jouken.kenkyu_kikan_name)

        self.driver.get(f"{self.config['JST_SOURCE']['URL']}?{url_params}")
        page_result_base = PageResultBase(self.driver)

        # JSTサイトへのURLリストを取得する
        if page_result_base.total_page_count:
            for page_no in range(1, page_result_base.total_page_count+1, int(self.config['JST_OPTION']['RESULTS_PER_PAGE'])):
                try:
                    url_params_with_page = url_params + "&" + "st=" + str(page_no)
                    self.driver.get(f"{self.config['JST_SOURCE']['URL']}?{url_params_with_page}")
                    page_jst_result_list = PageJstResultList(self.driver)

                    for project_data in page_jst_result_list.project_data_list:
                        ret_project = ProjectJst()
                        ret_project.jigyou_kubun_name = "(JST)科学技術振興機構"
                        ret_project.kenkyu_kadai_url = project_data['link']
                        # 研究課題名
                        ret_project.kenkyu_kadai_name = project_data['kenkyu_kadai_name']
                        # 研究機関名
                        if len(project_data['kenkyu_kikan_list']):
                            ret_project.kenkyu_kikan_name = "/".join(project_data['kenkyu_kikan_list'] + [""])
                        else:
                            ret_project.kenkyu_kikan_name = ""

                        # 研究期間(開始)
                        if 'kenkyu_start_date' in project_data:
                            match project_data['kenkyu_start_date']:
                                case str_date if re.fullmatch(r"\d{4}", str_date):
                                    ret_project.kenkyu_start_date = datetime.strptime(project_data['kenkyu_start_date'] + "-04-01", "%Y-%m-%d").date()
                                case str_date if re.fullmatch(r"\d{4}-\d{2}-\d{2}", str_date):
                                    ret_project.kenkyu_start_date = datetime.strptime(project_data['kenkyu_start_date'], "%Y-%m-%d").date()
                                case _:
                                    ret_project.kenkyu_start_date = None
                        else:
                            ret_project.kenkyu_start_date = None

                        # 研究期間(終了)
                        if 'kenkyu_end_date' in project_data:
                            match project_data['kenkyu_end_date']:
                                case str_date if re.fullmatch(r"\d{4}", str_date):
                                    ret_project.kenkyu_end_date = datetime.strptime(str(int(project_data['kenkyu_end_date'])+1) + "-03-31", "%Y-%m-%d").date()
                                case str_date if re.fullmatch(r"\d{4}-\d{2}-\d{2}", str_date):
                                    ret_project.kenkyu_end_date = datetime.strptime(project_data['kenkyu_end_date'], "%Y-%m-%d").date()
                                case _:
                                    ret_project.kenkyu_end_date = None
                        else:
                            ret_project.kenkyu_end_date = None

                        if len(project_data['kenkyu_daihyosha_list']):
                            self.logger.info(project_data['kenkyu_daihyosha_list'])
                            # 研究代表者名
                            ret_project.kenkyu_daihyosha_name =  project_data['kenkyu_daihyosha_list'][0]['name']
                            # 研究代表者所属機関
                            ret_project.kenkyu_daihyosha_syozoku_kikan_name =  project_data['kenkyu_daihyosha_list'][0]['syozokukikan_name']
                            # 研究代表者所属部署
                            ret_project.kenkyu_daihyosha_syozoku_busyo_name = "(役職に記載)"
                            # 研究代表者役職
                            ret_project.kenkyu_daihyosha_yakusyoku_name =  project_data['kenkyu_daihyosha_list'][0]['syozokukikan_name']
                            if len(project_data['kenkyu_daihyosha_list']) > 1:
                                # 研究代表者名
                                ret_project.kenkyu_daihyosha_name += " 他" + str(len(project_data['kenkyu_daihyosha_list'])-1) + "名"
                                # 研究代表者所属機関
                                ret_project.kenkyu_daihyosha_syozoku_kikan_name += " 他" + str(len(project_data['kenkyu_daihyosha_list'])-1) + "機関"
                                # 研究代表者役職
                                ret_project.kenkyu_daihyosha_yakusyoku_name += " 他" + str(len(project_data['kenkyu_daihyosha_list'])-1) + "役職"
                        else:
                            ret_project.kenkyu_daihyosha_name =  "(指定されていません)"
                            ret_project.kenkyu_daihyosha_syozoku_kikan_name =  "(指定されていません)"
                            ret_project.kenkyu_daihyosha_syozoku_busyo_name = "(指定されていません)"
                            ret_project.kenkyu_daihyosha_yakusyoku_name =  "(指定されていません)"

                        ret_projects.append(ret_project.to_dict())
                        
                except Exception as e:
                    self.logger.error(f"プロジェクトの抽出に失敗しました: {e}")
                    self.logger.error(f"失敗したプロジェクトリストのリンク先: {self.driver.current_url}")
                    continue
        
        return ret_projects

    def get_nested(self, data, path, default=None):
        """Nested dict safe getter.

        Args:
            data: dict-like object to traverse
            path: list of keys to descend
            default: value returned when any key is missing
        """
        current = data
        for key in path:
            if isinstance(current, dict) and key in current:
                current = current[key]
            else:
                return default
        return current

    def get_kaken_projects(self, params):
        ret = {}
        endpoint_url = self.config['CINII_OPENSEARCH']['URL']
        response = requests.get(endpoint_url, params=params)
        if response.status_code == 200:
            ret = xmltodict.parse(response.text)
        else:
            ret = None
        return ret 

    def output_projects(self, projects, directory, filebase, sheetname="一覧"):
        outdata = pd.DataFrame(projects)

        # 日付が小さい順にソートする
        outdata = outdata.sort_values('kenkyu_start_date', ascending=True)
        # 日付を「YYYY/MM/DD」形式の文字列に変換する
        outdata['kenkyu_start_date'] = pd.to_datetime(outdata['kenkyu_start_date'], errors='coerce').dt.strftime('%Y/%m/%d')
        outdata['kenkyu_end_date'] = pd.to_datetime(outdata['kenkyu_end_date'], errors='coerce').dt.strftime('%Y/%m/%d')

        # 特定の列を列名順に並び替える
        fixed_cols = list(outdata.columns[:13])
        sorted_cols = sorted(outdata.columns[13:])
        outdata = outdata[fixed_cols + sorted_cols]
        # 列名をリネームする(配分額以外)
        outdata = outdata.rename(columns={
            'jigyou_kubun_name': '事業名',
            'kenkyu_kadai_url': 'link',
            'kenkyu_kadai_name': '研究課題名',
            'kenkyu_kikan_name': '研究機関名',
            'kenkyu_start_date': '研究開始日',
            'kenkyu_end_date': '研究終了日',
            'kenkyu_daihyosha_name': '研究代表者',
            'kenkyu_daihyosha_syozoku_kikan_name': '所属機関',
            'kenkyu_daihyosha_syozoku_busyo_name': '部署',
            'kenkyu_daihyosha_yakusyoku_name': '役職',
        })
        # 列名をリネームする(配分額)
        outdata.columns = outdata.columns.str.replace('kenkyu_haibungaku_total_direct', '直接経費', regex=False)
        outdata.columns = outdata.columns.str.replace('kenkyu_haibungaku_total_indirect', '間接経費', regex=False)
        outdata.columns = outdata.columns.str.replace('kenkyu_haibungaku_total', '合計経費', regex=False)

        # 出力先
        os.makedirs(os.path.join(self.output_base, directory), exist_ok=True)
        output_path = os.path.join(self.output_base, directory, f"{filebase}.xlsx")

        # 長いURL対策(ExcelWriter)
        # with StyleFrame.ExcelWriter(output_path) as writer:
        with pd.ExcelWriter(output_path, engine='xlsxwriter') as writer:
            # sf = StyleFrame(outdata)
            outdata.to_excel(writer, sheet_name=sheetname, index=False)

            # 出力フォーマット設定
            workbook = writer.book
            worksheet = writer.sheets[sheetname]

            # 全ての列の幅を10に設定し、見出しは右寄せにする
            header_format = workbook.add_format({'align': 'left'})
            for col_num, value in enumerate(outdata.columns.values):
                worksheet.set_column(col_num, col_num, 12)
                worksheet.write(0, col_num, value, header_format) 

            # 金額フォーマット
            money_format = workbook.add_format({'num_format': '¥#,##0'})
            worksheet.set_column('K:ZZ', 12, money_format)

    def listup_kenkyu_kikan(self, directory):
        # 条件に合うプロジェクトが存在している研究機関のリスト
        output_folder = Path(os.path.join(self.output_base, directory))
        if output_folder.is_dir():
            self.logger.info("研究機関の所在地を特定しています")
            kenkyu_kikan_names = [f.name.replace(".xlsx", "") for f in output_folder.iterdir() if f.is_file()]
            kenkyu_kadai_links = [f'=HYPERLINK("./{directory}/{f.name}")' for f in output_folder.iterdir() if f.is_file()]

            kenkyu_kikan_place = []
            prefecture_names = []
            map_url = "https://www.google.com/maps/search/?api=1"
            for index, kenkyu_kikan_name in enumerate(kenkyu_kikan_names, start=1):
                while True:
                    try:
                        kenkyu_kikan_map_url = f"{map_url}&query={quote(kenkyu_kikan_name)}"
                        self.driver.get(kenkyu_kikan_map_url)
                        WebDriverWait(self.driver, 30).until(
                            lambda driver: driver.current_url != kenkyu_kikan_map_url
                        )
                        kenkyu_kikan_place.append(kenkyu_kikan_map_url)

                        if self.config['OPTION']['IDENTIFY_PREFECTURE'].lower() == "true":
                            match = re.search(r'!3d([^!]+)!4d([^!]+)', self.driver.current_url)
                            if match is not None and len(match.groups()) == 2:
                                lat, lon = map(float, match.groups())
                                point = Point(lon, lat)
                                ret = self.prefecture[self.prefecture.covers(point)]
                                if not ret.empty:
                                    prefecture_name = ret.iloc[0]["N03_001"]
                                    prefecture_names.append(prefecture_name)
                                else:
                                    prefecture_names.append("都道府県判定に失敗しています")
                            else:
                                prefecture_names.append("所在不明、または候補地が複数存在しています")
                        else:
                            prefecture_names.append("都道府県判定は無効です")

                        break

                    except Exception as e:
                        self.logger.error(f"所在地の特定に失敗しました: {e}")
                        self.logger.error(f"失敗した所在地のリンク先: {kenkyu_kikan_map_url}")
                        prefecture_names.append("所在地の特定に失敗しました")
                        self.driver.quit()
                        self.driver = webdriver.Edge(options=self.options)

                self.logger.info(f"{100*index/len(kenkyu_kikan_names):6.2f}% completed. {directory.upper()}: {kenkyu_kikan_name}")

            df = pd.DataFrame(
                {
                    "都道府県": prefecture_names,
                    "拠点地図": kenkyu_kikan_place,
                    "研究機関": kenkyu_kikan_names,
                    "研究課題": kenkyu_kadai_links
                }
            )
            df = df.sort_values('都道府県', ascending=True)
            df.to_excel(os.path.join(self.output_base,f"{directory}.xlsx"), sheet_name="一覧", index=False)

    def normalize(self, text):
        text = text.strip()

        # 除去したい法人格
        REMOVE_WORDS = self.load_names("./settings/normalize.yaml")

        for word in REMOVE_WORDS:
            text = text.replace(word, "")

        # 全角・半角スペース除去
        text = re.sub(r"\s+", "", text)
        # 半角英数・全角漢字に統一
        text = unicodedata.normalize("NFKC", text)

        # self.logger.info(f"{text}")

        return text

    def load_names(self, filename):
        with open(filename, "r", encoding="utf-8") as f:
            data = yaml.safe_load(f)

        return data

    def search_unified_names(self, kenkyu_kikan_yaml_path):
        THRESHOLD = 100
        self.df_corporate_resistry["name_norm"] = self.df_corporate_resistry["name"].swifter.apply(self.normalize)
        names = []
        for kenkyu_kikan_yaml in Path(kenkyu_kikan_yaml_path).glob("*.yaml"):
            kenkyu_kikan_list = self.load_names(kenkyu_kikan_yaml) 
            for index, name in enumerate(kenkyu_kikan_list, start=1):
                keys = {}
                keys["name"] = name
                keys["name_norm"] = self.normalize(name)
                matchs = process.extract_iter(
                    keys["name_norm"]
                    , self.df_corporate_resistry["name_norm"]
                    , scorer=fuzz.WRatio
                    , score_cutoff=THRESHOLD
                )
                for index, match in enumerate(matchs, start=0):
                    if index:
                        self.logger.info(f"全国に同名の研究機関が存在しています: {name}")
                        break
                    matched_norm, score, idx = match
                    keys["name_unified"] = self.df_corporate_resistry.loc[idx, "name"]
                    keys["corporate_number"] = self.df_corporate_resistry.loc[idx, "corporate_number"] 
                    keys["prefecture_name"] = self.df_corporate_resistry.loc[idx, "prefecture_name"]
                    keys["post_code"] = self.df_corporate_resistry.loc[idx, "post_code"]
                    keys["score"] = score
                    names.append(keys)

                self.logger.info(f"{100*index/len(kenkyu_kikan_list):6.2f}% completed. {kenkyu_kikan_yaml_path}/{kenkyu_kikan_yaml}: {name}")

        self.unified_kenkyu_kikan_list.extend(names)

        # 重複削除
        self.unified_kenkyu_kikan_list = list(
            {
                tuple(sorted(data.items())): data
                for data in self.unified_kenkyu_kikan_list
            }.values()
        )

with Main() as main:
    main.run()