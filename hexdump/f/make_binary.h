//*************************************************************
//* NAME        : makebinary.h
//* FUNCTION    : 16進ダンプ機能用ヘッダー
//* PROGRAMMED  : 15.07.14 Balto/藤本
//* HISTORY     :
//* ID --- DATE --- NAME  ---- NOTE ---------------------------
//*[PRJ000]15.07.14 Balto/藤本 新規作成
//*************************************************************

// プロトタイプ宣言
void MakeBinary(const char *spInput, const char *spOutput);
int LineCheck(std::string &sHexData, const std::string &sFormat);
