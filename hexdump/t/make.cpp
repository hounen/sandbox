//**************************************************************************
//* NAME        : make.cpp
//* FUNCTION    : バイナリファイル生成  
//* PROGRAMED   : 15.07.14 Balto/立石
//* HISTORY     : 
//* ID -- DATE -------- NOTE -----------------------------------------------
//*[PRJ000]15.07.14 Balto/立石  新規作成
//**************************************************************************
#include <iostream>
#include <string>
#include <fstream>
#include <ctype.h>

using namespace std ;

// プロトタイプ宣言
int LineCheck(string &sHexData, const string &sFormat) ;

//**************************************************************************
//* NAME         : MakeBinary
//* FUNCTION     : バイナリファイル生成
//* INPUT        : const char *spInput → 入力ファイル名
//*              : const char *spOutput → 出力ファイル名
//* OUTPUT       : バイナリファイル
//* RETURN       : 無し
//* PROGRAMMED   : 2015.07.14 Balto/立石
//* REMARKS      : 
//**************************************************************************
void MakeBinary(const char *spInput, const char *spOutput)
{
	// テキストモードで入力ファイルを開く
	ifstream ifs(spInput, ios::in) ;
	if (!ifs) { // オープン失敗か？
		throw ;
	}
	// 入力データの文字列を格納する配列を宣言
	string sFormat ; // http://vivi.dyndns.org/tech/cpp/string.html#array
	// 16進数文字のみの文字列を格納する配列を宣言
	string sHexData ;
	
	int iLineCount = 0 ;
int CheckNumber = 0 ;
	
	// 入力ファイルから文字列を行単位で読み込む
	while(getline(ifs,sFormat)) {
		// 行内の16進数文字だけをバイナリ変換し、文字列に格納する
		CheckNumber = LineCheck(sHexData, sFormat) ;
		iLineCount++ ;
		if(CheckNumber != 0) { // エラー発生によりループを抜ける
			break ; 
		}
	}
	
	ifs.close();
	// バイナリモードで出力ファイルを開く
	ofstream ofs(spOutput, ios::binary) ;
	if(!ofs){ // オープン失敗か？
		throw ;
	}
	// 出力ファイルに16進数文字のみの文字列をすべて書き込む
	ofs << sHexData ;
  cout << sHexData ;
	//エラー分岐
	switch (CheckNumber) {
		case 0 : // 正常終了
			throw ;
			break ;
		case 1 : // 入力行長エラー
			throw ;
			break ;
		case 2 : // 不正文字検出エラー
			throw ;
			break ;
		case 3 : // 16進数文字列長エラー
			throw ;
			break ;
	}

	ofs.close() ;
}

//**************************************************************************
//* NAME         : LineCheck
//* FUNCTION     : 入力行チェック
//* INPUT        : const string &sFormat 入力データの文字列
//* OUTPUT       : string &sHexData 16進文字のみの文字列
//* RETURN       : 0 → 正常終了
//*              : 1 → 入力行長エラー
//*              : 2 → 不正文字検出
//*              : 3 → 16進文字列長エラー
//* PROGRAMMED   : 2015.07.15 Balto/立石
//* REMARKS      : 
//**************************************************************************
int LineCheck(string &sHexData, const string &sFormat)
{
  //入力行長のチェック
	unsigned iLineSize = sFormat.size() ;
	
	if(iLineSize > 2048){
		return 1 ;
	}
	// 状態変数を初期化
	unsigned iState = 0 ;
	// カラムを初期化する
	unsigned iColumn = 0 ;
	// バイナリ変数を宣言する
	unsigned iBinaryData = 0 ;

	while(iColumn++ < iLineSize) {
		switch(iState) {
			case 0 : 
				switch(sFormat[iColumn-1]) {
					case '#'  : 
						break ;
					case '$'  : 
						iState = 2 ;
						continue ;
					case ' '  : 
						continue ;
					case '\t' : 
						continue ;
					case '\r' : 
						continue ;
					case '\n' : 
						continue ;
          default   :
						if (isxdigit(sFormat[iColumn-1])) {
							if ('0' <= sFormat[iColumn-1] && sFormat[iColumn-1] <= '9') {
								iBinaryData = sFormat[iColumn-1] - '0' ;
							}
							if ('A' <= sFormat[iColumn-1] && sFormat[iColumn-1] <= 'F') {
								iBinaryData = sFormat[iColumn-1] - 'A' + 10 ;
							}
							if ('a' <= sFormat[iColumn-1] && sFormat[iColumn-1] <= 'f') {
								iBinaryData = sFormat[iColumn-1] - 'a' + 10 ;
							}
							iBinaryData <<= 4 ; 
							iState = 1 ;
							continue ;
						}
						return 2 ;
						break ;
				}
				break ;
			case 1 :
				if (isxdigit(sFormat[iColumn-1])) {
						iBinaryData += iBinaryData ;
						sHexData += iBinaryData ; // http://www.cppll.jp/cppreference/cppstring_details.html#append
						iBinaryData = 0 ;
						iState = 0 ;
						continue ;
				}
				return 2;
				break ;
			case 2 :
				if(sFormat[iColumn-1] == ' ') {
					iState = 0;
				}
				continue ;
		}	
	}

	if (iState == 1) {
  	return 3 ;
	}
	return 0 ;
}
