//*************************************************************
//* NAME        : makebinary.cpp
//* FUNCTION    : バイナリファイル生成
//* PROGRAMMED  : 15.07.14 Balto/藤本
//* HISTORY     :
//* ID --- DATE --- NAME  ---- NOTE ---------------------------
//*[PRJ000]15.07.14 Balto/藤本 新規作成
//*************************************************************
#include <fstream>
#include <string>

#include "error.h"

using namespace std;

int LineCheck(std::string &sHexData, const std::string &sFormat);

//*------------------------------------------------------------
//* NAME        : MakeBinary
//* FUNCTION    : 
//* INPUT       : const char *spInput		: 入力ファイル名
//* OUTPUT      : const char *spOutput		: 出力ファイル名
//* RETURN      : 0
//* PROGRAMMED  : 15.07.14 Balto/藤本
//* REMARKS     :
//*------------------------------------------------------------
void MakeBinary(const char *spInput, const char *spOutput)
{	
	//---------------------------------------------------------
	// 入力ファイルをオープンする
	//---------------------------------------------------------
	ifstream ifs(spInput);
	if(!ifs){						// 入力ファイルオープン失敗？
		ClsError error(INPUT_OPEN_ERROR, 0);
		throw error;
	}

	string sBuf("");					// LineCheckで一時的に格納する文字列
	
	int iLineCount = 0;				// 行数カウンタ
	
	int iReturnValue = 0;			// LineCheckの戻り値を格納する変数

	//---------------------------------------------------------
	// 入力ファイルの終端まで
	//---------------------------------------------------------
	string sInputData;
	while(getline(ifs, sInputData)) {
		iLineCount++;
		
		iReturnValue = LineCheck(sBuf, sInputData);
		if(iReturnValue != 0){		// LineCheckが正常終了?
			break;
		}
	}

	//---------------------------------------------------------
	// 入力ファイルをクローズする
	//---------------------------------------------------------
	ifs.close();
	
	//---------------------------------------------------------
	// 出力ファイルをオープンする
	//---------------------------------------------------------
	ofstream ofs(spOutput, ios::binary);	
	if(!ofs){						// 出力ファイルオープン失敗？
		ClsError error(OUTPUT_OPEN_ERROR, 0);
		throw error;
	}

	//---------------------------------------------------------
	// 出力ファイルにバイナリ変換結果をすべて書き込む
	//---------------------------------------------------------
	ofs << flush << sBuf;
	
	//---------------------------------------------------------
	// 出力ファイルをクローズする
	//---------------------------------------------------------
	ofs.close();

	//---------------------------------------------------------
	// LineCheckの戻り値 エラー分岐
	//---------------------------------------------------------
	switch(iReturnValue){
	case 1:
		{
			ClsError error(ILLEGAL_CHARACTER_ERROR, iLineCount);
			throw error;
		}
		break;
		
	case 2:
		{
			ClsError error(HEX_LENGTH_ERROR, iLineCount);
			throw error;
		}
		break;
	case 3:
		{
			ClsError error(INPUT_LENGTH_ERROR, iLineCount);
			throw error;
		}
		break;
	default:
		;
		break;
	}
	

	
	
}

//*------------------------------------------------------------
//* NAME        : LineCheck
//* FUNCTION    : バイナリファイル生成 16進数文字以外を読み飛ばす
//* INPUT       : const string& sLine		: 入力データの文字列
//* OUTPUT      : string& sBinaryValue		: 16進数文字のみの文字列
//* RETURN      : 0	: 正常終了
//*				: 1	: 入力行長エラー
//*				: 2	: 不正文字検出
//*				: 3	: 16新文字列長エラー
//* PROGRAMMED  : 15.07.14 Balto/藤本
//* REMARKS     :
//*------------------------------------------------------------
int LineCheck(string& sBinaryValue, const string& sLine)
{
	//---------------------------------------------------------
	// 入力行長をチェックする
	//---------------------------------------------------------
	int iSizeMax = sLine.length();
	if(iSizeMax > 2048){
		return 1;
	}

	int iState = 0;							// 状態変数
	unsigned char ucBinaryData = 0x0000;	// バイナリ変数

	//---------------------------------------------------------
	// カラム < 入力行長
	//---------------------------------------------------------
	for(int iColumn = 0; iColumn < iSizeMax; iColumn++){
		if(iState == 0){		// すべての文字を受け付ける
			
			if(sLine[iColumn] == '#'){
				break;
			}

			if(sLine[iColumn] == '$'){
				iState = 2;					// 空白までを読み飛ばす状態に遷移
			}else if(isspace(sLine[iColumn])){			// 空白類文字?
				// 読み飛ばす
				;
			}else if(isxdigit(sLine[iColumn])){			// 16進数文字?
				// 16進数文字からバイナリへ変換する
				if(isdigit(sLine[iColumn])){			// 数値?
					ucBinaryData = sLine[iColumn] - '0';
				}else if(isupper(sLine[iColumn])){		// 大文字?
					ucBinaryData = sLine[iColumn] - 'A' + 10;
				}else{									// それ以外(小文字)
					ucBinaryData = sLine[iColumn] - 'a' + 10;
				}
				ucBinaryData = ucBinaryData << 4;		// 4bit左シフトしてバイナリ変数に加算する
				iState = 1;					// 16進数文字のみを受け付ける状態に遷移
			}else{		// それ以外なら不正文字
				return 2;
			}

		}else if(iState == 1){	// 16進数文字のみを受け付ける
			if(!isxdigit(sLine[iColumn])){	// 16進数文字?
				return 2;
			}
			// 16進数文字からバイナリへ変換し、バイナリ変数に加算する
			if(isdigit(sLine[iColumn])){				// 数値?
				ucBinaryData += sLine[iColumn] - '0';
			}else if(isupper(sLine[iColumn])){			// 大文字?
				ucBinaryData += sLine[iColumn] - 'A' + 10;
			}else{										// それ以外(小文字)
				ucBinaryData += sLine[iColumn] - 'a' + 10;
			}

			sBinaryValue += ucBinaryData;	// バイナリ変数をsBinaryValueに追加する

			ucBinaryData = 0x0000;		// バイナリ変数を初期化
			iState = 0;					// すべての文字を受け付ける状態に遷移
		
		}else if(iState == 2){	// 空白までを読み飛ばす
			if(sLine[iColumn] == ' '){	// 空白?
				iState = 0;				// すべての文字を受け付ける状態に遷移
			}
		}
	
	}	//for
	

	if(iState == 1){	// 16進数文字のみを受け付ける状態?
		return 3;		
	}

	return 0;
}
