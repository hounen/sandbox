//*************************************************************
//* NAME        : error.h
//* FUNCTION    : エラークラスヘッダー
//* PROGRAMMED  : 15.07.14 Balto/藤本
//* HISTORY     :
//* ID --- DATE --- NAME  ---- NOTE ---------------------------
//*[PRJ000]15.07.14 Balto/藤本 新規作成
//*************************************************************
#ifndef CLSERROR_H

#define CLSERROR_H
static const unsigned long INPUT_OPEN_ERROR			=	0x010001UL;
static const unsigned long INPUT_WRITE_ERROR		=	0x010002UL;
static const unsigned long INPUT_CLOSE_ERROR		=	0x010003UL;
static const unsigned long OUTPUT_OPEN_ERROR		=	0x020001UL;
static const unsigned long OUTPUT_WRITE_ERROR		=	0x020002UL;
static const unsigned long OUTPUT_CLOSE_ERROR		=	0x020003UL;
static const unsigned long ILLEGAL_CHARACTER_ERROR	=	0x030001UL;
static const unsigned long HEX_LENGTH_ERROR			=	0x030002UL;
static const unsigned long INPUT_LENGTH_ERROR		=	0x030003UL;


class ClsError : public exception
{
private:
	int iCode;
	int iLine;
	
public:	
	ClsError(int code, int line);
	~ClsError();
	void ShowError();
};


#endif // CLSERROR_H







