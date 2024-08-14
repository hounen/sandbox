//**************************************************************************
//* NAME        : main.cpp
//* FUNCTION    : メイン  
//* PROGRAMED   : 15.07.14 Balto/立石
//* HISTORY     : 
//* ID -- DATE -------- NOTE -----------------------------------------------
//*[PRJ000]15.07.14 Balto/立石  新規作成
//**************************************************************************
#include <iostream>
#include <string>

using namespace std ;

// プロトタイプ宣言
int  ParameterCheck(int argc, char *argv[]) ;
void Usage(void) ;
void Dump(const char *spInput, const char *spOutput) ;
void MakeBinary(const char *spInput, const char *spOutput) ;

//**************************************************************************
//* NAME         : Main
//* FUNCTION     : 実行時に起動され、関数を呼ぶ
//* INPUT        : argc → コマンドライン引数の個数
//*              : *argv[] → コマンドライン引数の内容
//* OUTPUT       : 無し
//* RETURN       :  0 → 正常終了 
//*              : -1 → 異常終了
//* PROGRAMMED   : 2015.07.14 Balto/立石
//* REMARKS      : 
//**************************************************************************
int main(int argc, char *argv[])
{
	// パラメタをチェックし、機能を判別する
	int iFuncNumber = ParameterCheck(argc, argv) ;	
	
	switch (iFuncNumber) { //機能の切り分け
		case 0 : // 使用法表示機能
			Usage() ;
			break ;
		case 1 : // 16進ダンプ機能
			Dump(argv[2], argv[3]) ;
			break ;
		case 2 : // バイナリファイル生成機能
		  MakeBinary(argv[2], argv[3]) ;
			break ;
	}
	return 0 ;
}

//**************************************************************************
//* NAME         : ParameterCheck
//* FUNCTION     : パラメタ(オプションを含む)の判定を行う
//* INPUT        : argc → コマンドライン引数の個数
//*              : *argv[] → コマンドライン引数の内容
//* OUTPUT       : 無し
//* RETURN       : FuncNumber = 
//*              : 0 → 使用法表示機能
//*              : 1 → 16進ダンプ機能
//*              : 2 → バイナリファイル生成機能
//* PROGRAMMED   : 15.07.14 Balto/立石
//* REMARKS      : 
//**************************************************************************
int ParameterCheck(int argc, char *argv[])
{
	int iFuncNumber = 0 ;
	
	if (argc >= 2) { // オプションがあるか？
		string Option(argv[1]) ;
		if(Option == "/D") { // 16進ダンプ機能か？
			if(argc == 4) { 
				iFuncNumber = 1 ;
			}
		}
		else if(Option == "/M") { // バイナリファイル生成機能か？
			if(argc == 4){ 
				iFuncNumber = 2 ;
			}
		}
	}
	return iFuncNumber ;
}
