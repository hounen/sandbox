//*************************************************************
//* NAME        : error.cpp
//* FUNCTION    : エラークラス
//* PROGRAMMED  : 15.07.14 Balto/藤本
//* HISTORY     :
//* ID --- DATE --- NAME  ---- NOTE ---------------------------
//*[PRJ000]15.07.14 Balto/藤本 新規作成
//*************************************************************
#include <iostream>
#include <iomanip>

#include "error.h"

using namespace std;

ClsError::ClsError(int code, int line)
	:iCode(code)
	,iLine(line)
{	
}

ClsError::~ClsError()
{
	
}

void ClsError::ShowError()
{
	
struct StrMessage{
	unsigned long m_code;
	const char* m_message;
	StrMessage(unsigned long code, const char* message)
		:m_code(code)
		,m_message(message)
	{
	}
};

static const StrMessage msg[] = {
		{StrMessage(INPUT_OPEN_ERROR,			"入力ファイルオープンエラー")},
		{StrMessage(INPUT_WRITE_ERROR,			"入力ファイルライトエラー")},
		{StrMessage(INPUT_CLOSE_ERROR,			"入力ファイルクローズエラー")},
		{StrMessage(OUTPUT_OPEN_ERROR,			"出力ファイルオープンエラー")},
		{StrMessage(OUTPUT_WRITE_ERROR,			"出力ファイルオープンエラー")},
		{StrMessage(OUTPUT_CLOSE_ERROR,			"出力ファイルオープンエラー")},
		{StrMessage(ILLEGAL_CHARACTER_ERROR,	"不正文字検出")},
		{StrMessage(HEX_LENGTH_ERROR,			"16進数文字列長エラー")},
		{StrMessage(INPUT_LENGTH_ERROR,			"入力行長エラー")},
		{StrMessage(0,							"")},
};

	const StrMessage* ptr = msg;
	for(; ptr->m_code ; ptr++){
		if (ptr->m_code == iCode){
			break;
		}
	}

	if(iLine == 0){
		cout << "#" << setw(6) << setfill('0') << hex << iCode << " " << ptr->m_message << endl;	
	}else{
		cout << "#" << setw(6) << setfill('0') << hex << iCode << " " << ptr->m_message << " line:" << iLine << endl;
	}
}

