//**************************************************************************
//* NAME        : dump.cpp
//* FUNCTION    : 16進ダンプ  
//* PROGRAMED   : 15.07.14 Balto/立石
//* HISTORY     : 
//* ID -- DATE -------- NOTE -----------------------------------------------
//*[PRJ000]15.07.14 Balto/立石  新規作成
//**************************************************************************
#include <iostream>
#include <string>
#include <fstream>
#include <iomanip>

using namespace std ;

//**************************************************************************
//* NAME         : Dump
//* FUNCTION     : 16進ダンプ
//* INPUT        : 入力ファイル名、出力ファイル名
//* OUTPUT       : 16進ダンプファイル
//* RETURN       : なし
//* PROGRAMMED   : 2015.07.14 Balto/立石
//* REMARKS      : 
//**************************************************************************
void Dump(const char *spInput, const char *spOutput) 
{
	// バイナリモードで入力ファイルを開く
	ifstream ifs(spInput, ios::binary) ; 
	if (!ifs) { // オープン失敗か？
		throw ;
	}
	
	// 入力ファイルをすべて読み込む
	string BinaryData ;
	ifs >> BinaryData ; 

	ifs.close() ;
	 
	// テキストモードで出力ファイルを開く
	ofstream ofs(spOutput, ios::out) ;
	if (!ofs) { // オープン失敗か？
		throw ;
	}
	// タイトル行を書き込む、改行はなし
	ofs << "#ADDRESS   0 1  2 3  4 5  6 7   8 9  A B  C D  E F" ;
	
	int BinaryDataSize = BinaryData.size() ;

	for (int CharCount = 0 ; CharCount != BinaryDataSize ; CharCount++) { 
		if( CharCount % 16 == 0) {
			// 16バイト単位内アドレスを書き込む
			ofs << "\n$" << hex << setw(8) << setfill('0') << CharCount ;
		}
		if (CharCount % 16 == 8) {
			// 8バイト分出力後に空白を書き込む
			ofs << " " ;
		}
		if (CharCount % 2 == 0) { 
			// 2バイト単位で空白を書き込む
			ofs << " " ;
		}
		// 16進ダンプ結果を書き込む
		ofs << setw(2) << setfill('0') << hex << uppercase << (unsigned)BinaryData[CharCount] ;
	}
	// ループ内で改行してないので、改行しておく
	ofs << "\n" ;  
	ofs.close() ;
}
