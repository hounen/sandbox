//*************************************************************
//* NAME        : main.cpp
//* FUNCTION    : メイン処理
//* PROGRAMMED  : 15.07.14 Balto/藤本
//* HISTORY     :
//* ID --- DATE --- NAME  ---- NOTE ---------------------------
//*[PRJ000]15.07.14 Balto/藤本 新規作成
//*************************************************************
#include <string>
#include <iostream>

#include "error.h"

using namespace std;

/* プロトタイプ宣言 */
void Usage();
int ParameterCheck(int argc, char* argv[]);
extern void Dump(const char *spInput, const char *spOutput);
extern void MakeBinary(const char *spInput, const char *spOutput);

//*------------------------------------------------------------
//* NAME        : main
//* FUNCTION    : 
//* INPUT       : int argc      : パラメタの個数
//*             : char *argv[]  : パラメタの内容
//* OUTPUT      : なし
//* RETURN      : 0
//* PROGRAMMED  : 15.07.14 Balto/藤本
//* REMARKS     :
//*------------------------------------------------------------
int main(int argc, char *argv[])
{

	try{
		//---------------------------------------------------------
		// パラメタを判定する
		//---------------------------------------------------------
		int iParameterValue = ParameterCheck(argc, argv);
		switch(iParameterValue){
		case 0:
			Usage();
			break;
		case 1:
			Dump(argv[2], argv[3]);
			break;
		case 2:
			MakeBinary(argv[2], argv[3]);
			break;
		default:
			;
			break;
		}

	}catch(ClsError& ex){
		ex.ShowError();
	}

	return 0;
}

//*------------------------------------------------------------
//* NAME        : ParameterCheck
//* FUNCTION    : パラメタ判定
//* INPUT       : int argc		: パラメタの個数
//*				: char *argv[]	: パラメタの内容
//* OUTPUT      : なし
//* RETURN      : 0	(使用法表示)
//*				: 1	(16進ダンプ)
//*				: 2	(バイナリファイル生成)
//* PROGRAMMED  : 15.07.14 Balto/藤本
//* REMARKS     :
//*------------------------------------------------------------
int ParameterCheck(int argc, char* argv[])
{
	int iReturnValue = 0;

	//---------------------------------------------------------
    // パラメタの個数を判定する
    //---------------------------------------------------------
	if(argc >= 2){					// オプションがあるか？
		string str(argv[1]);
		if(str == "/D"){			// オプションが16進ダンプ?
			if(argc == 4){			// 16進ダンプに必要なパラメタ数?
				iReturnValue  = 1;
			}
		}else if(str == "/M"){		// オプションがバイナリファイル生成?
			if(argc == 4){			// バイナリファイル生成に必要なパラメタ数?
				iReturnValue = 2;
			}
		}
	}

	return iReturnValue;
}

//*------------------------------------------------------------
//* NAME        : Usage
//* FUNCTION    : 使用法を表示する
//* INPUT       : なし
//* OUTPUT      : なし
//* RETURN      : なし
//* PROGRAMMED  : 15.07.14 Balto/藤本
//* REMARKS     :
//*------------------------------------------------------------
void Usage()
{
	//---------------------------------------------------------
    // 使用法を1行ずつ表示する
    //---------------------------------------------------------
	cout << "=== HexDump Ver1.0(11.06.03) ===" << endl;
	cout << "Function:" << endl;
	cout << "    指定ファイルの１６進ダンプおよび逆変換を行う" << endl;
	cout << "Usage   : HexDump [/[D|M]] (InputFile) (OutputFile)" << endl;
	cout << "Options : /D = (InputFile)の１６進ダンプを行い(OutputFile)に出力する" << endl;
	cout << "          /M = １６進ダンプ結果(InputFile)を読込み、" << endl;
	cout << "               バイナリファイルに復元し(OutputFile)に出力する" << endl;
	cout << "Params  : (InputFile) = 入力ファイル名" << endl;
	cout << "          (OutputFile)= 出力ファイル名" << endl;
	cout << "Example :" << endl;
	cout << "  HexDump /D File.bin File.txt    ... File.bin を読込み" << endl;
	cout << "                                      ダンプ結果を File.txt に出力する" << endl;
	cout << "  HexDump /M File.txt File.bin    ... File.txt を読込み" << endl;
    cout << "                                      バイナリファイル File.bin に出力する" << endl;
}


