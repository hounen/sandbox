# def update_jigyou_kubun_file(self):
#     # 事業別
#     if self.config['OPTION']['JIGYOU_KUBUN_SYNC'].lower() == "true":
#         while True:
#             answer = input(f"事業区分リスト{self.config['SEARCH']['JIGYOU_KUBUN_FILE']}を更新(*上書き)しますか? (y/n): ")
#             if answer.lower() == "y":
#                 print("更新中...")
#                 self.driver.get(self.config['KAKEN_SOURCE']['JIGYOU_KUBUN_URL'])
#                 page_jigyou_kubun_group = PageJigyouKubunGroup(self.driver)
#                 jigyou_kubun_list = page_jigyou_kubun_group.to_list()
#                 with open(self.config['SEARCH']['JIGYOU_KUBUN_FILE'], 'w', encoding='utf-8') as f:
#                     yaml.safe_dump(jigyou_kubun_list, f, allow_unicode=True, sort_keys=False)
#                 print("事業区分リストを再作成しました")
#                 break
#             elif answer.lower() == "n":
#                 break
#             else:
#                 print("y または n を入力してください")
# 
#     with open(self.config['SEARCH']['JIGYOU_KUBUN_FILE'], encoding='utf-8') as file:
#         target_jigyou_kubun = yaml.safe_load(file)
#     
#     return target_jigyou_kubun 
# 
# def update_suitei_bunya_file(self):
#     # 分野別
#     if self.config['OPTION']['SUITEI_BUNYA_SYNC'].lower() == "true":
#         while True:
#             answer = input(f"研究分野リスト{self.config['SEARCH']['SUITEI_BUNYA_FILE']}を更新(*上書き)しますか? (y/n): ")
#             if answer.lower() == "y":
#                 print("更新中...")
#                 self.driver.get(self.config['KAKEN_SOURCE']['SUITEI_BUNYA_URL'])
#                 page_suitei_bunya = PageSuiteiBunya(self.driver)
#                 suitei_bunya_list = page_suitei_bunya.to_list()
#                 with open(self.config['SEARCH']['SUITEI_BUNYA_FILE'], 'w', encoding='utf-8') as f:
#                     yaml.safe_dump(suitei_bunya_list, f, allow_unicode=True, sort_keys=False)
#                 print("研究分野リストを再作成しました")
#                 break
#             elif answer.lower() == "n":
#                 break
#             else:
#                 print("y または n を入力してください")
# 
#     with open(self.config['SEARCH']['SUITEI_BUNYA_FILE'], encoding='utf-8') as file:
#         target_suitei_bunya = yaml.safe_load(file)
#     
#     return target_suitei_bunya 

# def export_results_by_prefecture(self, project_name, activate=False):
#     ### phase 4 研究機関別にプロジェクト一覧をエクセルに出力
#     self.logger.info(f"({project_name})調査結果を出力しています...")
#     if len(self.projects):
#         kenkyu_daihyosha_syozoku_kikan_groups = defaultdict(list)
#         for project in self.projects:
#             kenkyu_daihyosha_syozoku_kikan_groups[project['kenkyu_daihyosha_syozoku_kikan_name']].append(project)

#         kenkyu_daihyosha_syozoku_kikan_names = kenkyu_daihyosha_syozoku_kikan_groups.keys()
#         for kenkyu_daihyosha_syozoku_kikan_name in kenkyu_daihyosha_syozoku_kikan_names:
#             output_directory = f"{project_name}"
#             output_filebase = re.sub(r'[\s\\/:*?"<>|]', '', kenkyu_daihyosha_syozoku_kikan_name)
#             output_sheetname = "一覧" # self.project_kensaku_jouken.kenkyu_kikan_name
#             if activate:
#                 for unified_kenkyu_kikan in self.unified_kenkyu_kikan_list:
#                     if unified_kenkyu_kikan['name'] == kenkyu_daihyosha_syozoku_kikan_name:
#                         output_directory = f"{project_name}/{unified_kenkyu_kikan['prefecture_name']}"
#                         output_filebase = unified_kenkyu_kikan['name_unified']
#                         break

#             self.output_projects(kenkyu_daihyosha_syozoku_kikan_groups[kenkyu_daihyosha_syozoku_kikan_name], output_directory, output_filebase, output_sheetname)

#         # 一覧も出力
#         df = pd.DataFrame({"研究機関": kenkyu_daihyosha_syozoku_kikan_names})
#         df.to_excel(f"{os.path.join(self.output_base, project_name)}.xlsx", index=False)
#     else:
#         self.logger.info("検索条件のプロジェクトはありません")
#      ...


        # ### phase 2 研究機関名マッチング
        # if self.config['OPTION']['ZENKOKU_KENKYU_KIKAN_SYNC'].lower() == "true":
        #     self.logger.info("全国の法人情報を取得しています...")
        #     for prefecture in self.load_names("./settings/prefecture.yaml"):
        #         self.logger.info(prefecture)
        #         self.df_corporate_resistry = pd.concat(
        #             [self.df_corporate_resistry, self.corporate_resistry_client.fetch(prefecture)]
        #             , ignore_index=True
        #         )
        #     self.logger.info("全国の法人名と検索用研究機関名のマッチングをしています...")
        #     self.search_unified_names("./settings/extract_kenkyu_kikan")
        #     ...

        #     with open(f"./cache/unified_kenkyu_kikan.yaml", "w", encoding="utf-8") as f:
        #         yaml.dump(self.unified_kenkyu_kikan_list, f, allow_unicode=True, sort_keys=False)

        #     self.unified_kenkyu_kikan_list = self.load_names(f"./cache/unified_kenkyu_kikan.yaml")
