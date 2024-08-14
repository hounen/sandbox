//**************************************************************************
//* NAME        : usage.cpp
//* FUNCTION    : 使用法表示  
//* PROGRAMED   : 15.07.14 Balto/立石
//* HISTORY     : 
//* ID -- DATE -------- NOTE -----------------------------------------------
//*[PRJ000]15.07.14 Balto/立石  新規作成
//**************************************************************************
#include <iostream>

using namespace std ;

//**************************************************************************
//* NAME         : Usage
//* FUNCTION     : 使用法表示
//* INPUT        : 無し
//* OUTPUT       : コンソールに使用法を表示
//* RETURN       : なし
//* PROGRAMMED   : 2015.07.14 Balto/立石
//* REMARKS      : 
//**************************************************************************
void Usage(void)
{
	cout << "=== HexDump Ver1.0(11.06.03) ===\n" ;
	cout << "Function\n" ;
	cout << "    指定ファイルの１６進ダンプおよび逆変換を行う\n" ;
	cout << "Usage   : HexDump [/[D|M]] (InputFile) (OutputFile)\n" ;
	cout << "Options : /D = (InputFile)の１６進ダンプを行い(OutputFile)に出力する\n" ;
	cout << "          /M = １６進ダンプ結果(InputFile)を読込み、\n" ;
	cout << "               バイナリファイルに復元し(OutputFile)に出力する\n" ;
	cout << "Params  : (InputFile) = 入力ファイル名\n" ;
	cout << "		   (OutputFile)= 出力ファイル名\n" ;
	cout << "Example :\n" ;
	cout << "  HexDump /D File.bin File.txt    ... File.bin を読込み\n" ;
	cout << "                                      ダンプ結果を File.txt に出力する\n" ;
	cout << "  HexDump /M File.txt File.bin    ... File.txt を読込み\n" ;
	cout << "                                      バイナリファイル File.bin に出力する\n" ;
}