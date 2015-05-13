/*----------------------------------------------------------------------------/
/  TT - Tiny Terminal  R0.3a.3 (C)ChaN, 2005-2014
/                      extra features from Martin Boekhoff 2012-2014
/-----------------------------------------------------------------------------/
/ TT is a simple terminal program for embedded projects. It is a free software
/ opened under license policy of GNU GPL.

   Copyright (C) 2013, ChaN, all right reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in
     the documentation and/or other materials provided with the
     distribution.

   * Neither the name of the copyright holders nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.

  08.05.2014 Martin Boekhoff
    added Alt+G support.
    added Alt-Z support.
	merged Version from February 2014 with Alt Up Down Left Right support.
  */

#define VERSION_STR "R0.3a.3"

#include <windows.h>
#include <commdlg.h>
//#include <winuser.h> // uncommented because of tcc  MO
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "tt.h"


#define	INIFILE		"tt.ini"
#ifndef _MAX_PATH 
#define _MAX_PATH 255 // needed by tcc  MBo
#endif
#define TMR_HANG	300	/* Hang-up (DTR=OFF) time */
#define TMR_BREAK	300	/* Break (TXD='0') time */

/* Keyboard/Internal command */
#define	KCMD_EXIT	1
#define	KCMD_SWLOG	2
#define KCMD_HANG	3
#define KCMD_BREAK	4
#define	KCMD_VIEW	5
#define	KCMD_BINARY	6
#define	KCMD_XMODEM	7
#define	KCMD_STOP	8
#define KCMD_BPSUP	9
#define KCMD_BPSDN	10
#define KCMD_PORTUP	11
#define KCMD_PORTDN	12
#define	KCMD_SWLOG2	13 /* MBo */
#define	KCMD_TIME	14 /* MBo */
#define KCMD_PARITY 15

#define OPEN_FIRST	20
#define RCVR_EXIT	21
#define RCVR_HALT	22
#define RCVR_HALTING	23

/* XMODEM control chars */
#define	SOH		0x01
#define	EOT		0x04
#define	ACK		0x06
#define	NAK		0x15
#define	CAN		0x18

/* View Modes - (future releases will scan for plugin dlls that add special views) */
#define VIEW_TTY 0
#define VIEW_HEX 1

/* Handles */
volatile HANDLE hComm = INVALID_HANDLE_VALUE;	/* Handle of open comm port */
volatile HANDLE hLog = INVALID_HANDLE_VALUE;	/* Handle of open log file */
HANDLE hTransmit = INVALID_HANDLE_VALUE;		/* Handle of transmission file */

HANDLE hKey, hScreen, hRcvrThread;				/* Handles of console and background thread */
CONSOLE_SCREEN_BUFFER_INFO ScreenInfo;			/* Original screen attrribute */

BOOL lineBegin = 1;                             /* For detection the start of a new line. MO */

volatile int RcvrCmd;	/* Command to background thread */
volatile int nTxb;
volatile char Rxc, Xmode, Xseq;
char Txb[256];
BYTE Rxb[1024];

volatile BYTE view;
volatile BOOL fTime;		/* Print Time for every Line MBo */
volatile DWORD DumpAddr;	/* Dump address */

/* Auto transmission controls */
int AutoXmit, FileSize, FilePtr;

/* File/Title strings */
char sTitle[256], sLogFileTitle[_MAX_PATH];
char sLogFile[_MAX_PATH], sOpenFile[_MAX_PATH];

/* Current port settings */
DCB comDcb;
int comPort = 1;
int comBps = 9600;
int comData = 8;
int comStop = ONESTOPBIT;
int comParity = NOPARITY;
BOOL comCtsflow = FALSE;
int comPol = 0;		/* Signal polarity - b0:Invert DTR, b1:Invert DSR, b2:Invert TXD (no transmission) */
int comHelp = 1;
char viewNameHex[] = "HEX";
char viewNameTty[] = "TTY";
char *comViewName = viewNameTty;

const char Usage1[] =
	"TT - Tiny Terminal "VERSION_STR" (C)ChaN, 2014\n\n"
	"Command line parameters:\n"
	" port=1,n81,9600 : Initial port number, format, bit rate\n"
	" bps=9600        : Bit rate\n"
	" flow=on         : RTS/CTS flow control (on,off)\n"
    " pol=0           : Invert outputs (b0:DTR, b1:RTS, b2:TXD w/o tx)\n"
	" log=<file>      : Initial log file\n"
	" help=1          : Display keyboard command at start-up\n"
	" *These options can also be put in the \"tt.ini\".\n\n"
	;

const char Usage2[] =
	"Keyboard command:\n"
	" Alt-X : Exit program\n"
	" Alt-L : Start/Stop logging to a file\n"
	" Alt-G : Start/Stop with predefined Filename\n" /* MBo */
	" Alt-V : Switch view mode, TTY and HEX\n"
	" Alt-T : Transmit a file as byte stream\n"
	" Alt-Y : Transmit a file in XMODEM\n"
	" Alt-Z : Timestamp for every Line\n"  /* MBo */
	" Alt-H : Hang-up (Invert DTR for 300ms)\n"
	" Alt-B : Break (Set TXD '0' for 300ms)\n"
	" Alt-P : Change parity (cycle beetween NONE, ODD, EVEN)\n"  /* MBo */
	" Alt-<nums> : Transmit a byte by number (e.g. 0 sends a '\\0', 122 sends a 'z')\n"
	" Alt-Up/Down : Change bit rate\n"
	" Alt-Left/Right : Change port number\n"
	;

void set_title (void)
{
	char frm[4];


	frm[0] = (comParity == ODDPARITY) ? 'O' : ((comParity == EVENPARITY) ? 'E' : 'N');
	frm[1] = comData + '0';
	frm[2] = (comStop == TWOSTOPBITS) ? '2' : '1';
	frm[3] = 0;
	if (hComm != INVALID_HANDLE_VALUE) /* Print config first MBo */
		sprintf(sTitle, "[COM%u:%s:%ubps] %s - TinyTerminal "VERSION_STR, comPort, frm, comBps, comViewName);
	else
		sprintf(sTitle, "[COM%u(not opened)] TinyTerminal "VERSION_STR, comPort);

	if (hLog != INVALID_HANDLE_VALUE)
		sprintf(sTitle + strlen(sTitle), " - %s", sLogFileTitle);

	if (AutoXmit)
		sprintf(sTitle + strlen(sTitle), " [Send: %u%%]", 100 * FilePtr / FileSize);

	SetConsoleTitle(sTitle);
}




UINT CALLBACK ofnhook (HWND hdlg, UINT msg, WPARAM wp, LPARAM lp)
{
	if (msg == WM_INITDIALOG)
		SetForegroundWindow(GetParent(hdlg));

	return 0;
}



void switch_logging (int type)  /* type for logging without File-Open-Dialog */
{
	OPENFILENAME lfn;


	if (hLog != INVALID_HANDLE_VALUE) {
		CloseHandle(hLog);
		hLog = INVALID_HANDLE_VALUE;
	}
	else if (type == 0) {
		lfn.lStructSize			= sizeof(OPENFILENAME);
		lfn.Flags				= OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_EXPLORER | OFN_ENABLEHOOK;
		lfn.hwndOwner			= FindWindow(NULL, sTitle);
		lfn.lpstrTitle			= "Open Logfile";
		lfn.lpstrInitialDir		= NULL;
		lfn.lpstrFilter			= "Log Files (*.txt)\0*.txt\0All Files (*.*)\0*.*\0\0";
		lfn.lpstrCustomFilter	= NULL;
		lfn.nFilterIndex		= 0;
		lfn.lpstrDefExt			= "txt";
		lfn.lpstrFile			= sLogFile;
		lfn.nMaxFile			= sizeof sLogFile;
		lfn.lpstrFileTitle		= sLogFileTitle;
		lfn.nMaxFileTitle		= sizeof sLogFileTitle;
		lfn.lpfnHook			= ofnhook;
		if (GetSaveFileName(&lfn)) {
			hLog = CreateFile(sLogFile, GENERIC_WRITE, 0, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
			if (hLog != INVALID_HANDLE_VALUE)
				SetFilePointer(hLog, 0, NULL, FILE_END);
		}
	} else { /* create the filename from current date and time   MO  */
        SYSTEMTIME st;
        GetLocalTime(&st);
        snprintf(sLogFileTitle, sizeof sLogFileTitle, "log_%02d-%02d-%02d_%02d.%02d.txt", st.wYear-2000, st.wMonth, st.wDay, st.wHour, st.wMinute);
        hLog = CreateFile(sLogFileTitle, GENERIC_WRITE, 0, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        if (hLog != INVALID_HANDLE_VALUE)
            SetFilePointer(hLog, 0, NULL, FILE_END);
    }
	set_title();
}




void stop_transmisson (void)
{
	CloseHandle(hTransmit);
	hTransmit = INVALID_HANDLE_VALUE;
	AutoXmit = 0;
	set_title();
	MessageBeep(MB_OK);
}




int create_sum(BYTE *buf, int cnt, int mode)
{
	int n, i;
	DWORD sum = 0;


	if (mode == 1) {
		for (i = 0; i < cnt; i++)
			sum += buf[i];
		buf[i] = (BYTE)sum;
		cnt += 1;
	}
	if (mode == 2) {
		for (i = 0; i < cnt; i++) {
			sum |= buf[i];
			for (n = 0; n < 8; n++) {
				sum <<= 1;
				if (sum & 0x1000000) sum ^= 0x1102100;
			}
		}
		buf[i++] = (BYTE)(sum >> 16);
		buf[i++] = (BYTE)(sum >> 8);
		cnt += 2;
	}

	return cnt;
}




void start_transmisson (int cmd)
{
	OPENFILENAME lfn;
	BY_HANDLE_FILE_INFORMATION finfo;


	if (hComm == INVALID_HANDLE_VALUE || AutoXmit || (comData != 8 && cmd == KCMD_XMODEM)) {
		MessageBeep(MB_OK);
		return;
	}

	lfn.lStructSize			= sizeof (OPENFILENAME);
	lfn.Flags				= OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_EXPLORER | OFN_ENABLEHOOK;
	lfn.hwndOwner			= FindWindow(NULL, sTitle);
	lfn.lpstrTitle			= (cmd == KCMD_XMODEM) ? "Transmit a file in XMODEM" : "Transmit a file as byte stream";
	lfn.lpstrInitialDir		= NULL;
	lfn.lpstrFilter			= "All Files (*.*)\0*.*\0\0";
	lfn.lpstrCustomFilter	= NULL;
	lfn.nFilterIndex		= 0;
	lfn.lpstrDefExt			= NULL;
	lfn.lpstrFile			= sOpenFile;
	lfn.nMaxFile			= sizeof sOpenFile;
	lfn.lpstrFileTitle		= NULL;
	lfn.nMaxFileTitle		= 0;
	lfn.lpfnHook			= ofnhook;
	if (GetOpenFileName(&lfn)) {
		hTransmit = CreateFile(sOpenFile, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
		if (hTransmit != INVALID_HANDLE_VALUE) {
			GetFileInformationByHandle(hTransmit, &finfo);
			FileSize = finfo.nFileSizeLow;
			FilePtr = 0;
			AutoXmit = cmd;
			Xmode = 0;
			set_title();
		}
	}
}




BOOL init_port (HANDLE h)
{
	COMMTIMEOUTS tmo = { 0, 0, 10, 1, 10};	/* Rx:10ms, Tx:n+10ms */
	DCB dcb;


	dcb.DCBlength = sizeof dcb;
	if (GetCommState(h, &dcb)) {
		dcb.BaudRate = comBps;
		dcb.ByteSize = comData;
		dcb.StopBits = comStop;
		dcb.Parity = comParity;
		dcb.fParity = FALSE;
		dcb.fOutxCtsFlow = (comPol & 2) ? FALSE : comCtsflow;
		dcb.fOutxDsrFlow = FALSE;
		dcb.fOutX = FALSE;
		dcb.fInX = FALSE;
		dcb.fRtsControl = (comPol & 2) ? RTS_CONTROL_DISABLE : (comCtsflow ? RTS_CONTROL_HANDSHAKE : RTS_CONTROL_ENABLE);
		dcb.fDtrControl = (comPol & 1) ? DTR_CONTROL_DISABLE : DTR_CONTROL_ENABLE;
		dcb.fDsrSensitivity = FALSE;
		dcb.fNull = FALSE;
		dcb.fAbortOnError = FALSE;
		for (;;) {
			if (SetCommState(h, &dcb)) {
				EscapeCommFunction(h, (comPol & 1) ? SETDTR : CLRDTR);
				Sleep(100);
				PurgeComm(h, PURGE_RXABORT|PURGE_RXCLEAR);
				EscapeCommFunction(h, (comPol & 1) ? CLRDTR : SETDTR);
				if (comPol & 4) EscapeCommFunction(h, SETBREAK);
				if (SetCommTimeouts(h, &tmo)) {
					comBps = dcb.BaudRate;
					memcpy(&comDcb, &dcb, sizeof dcb);
					nTxb = 0;
					return TRUE;
				}
			}
			if (dcb.BaudRate <= 115200 && dcb.BaudRate >= 300) break;
			dcb.BaudRate = 115200;	/* Fall back if high bps is rejected */
		}
	}
	return FALSE;
}



void open_port (int cmd)
{
	int pn, dir;
	HANDLE h;
	char str[20];


	switch (cmd) {
	case KCMD_PORTUP:	/* Next upper port number */
		dir = 1;
		break;
	case KCMD_PORTDN:	/* Next lower port number */
		dir = -1;
		break;
	case OPEN_FIRST:	/* Open current port */
		sprintf(str, "\\\\.\\COM%u", comPort);
		h = CreateFile(str, GENERIC_READ|GENERIC_WRITE, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
		if (h != INVALID_HANDLE_VALUE) {
			if (init_port(h))
				hComm = h;
			else
				CloseHandle(h);
		}
		return;
	default:
		return;
	}

	if (AutoXmit) return;

	RcvrCmd = RCVR_HALT;	/* Get background thread away from comm function */
	while (RcvrCmd != RCVR_HALTING) Sleep(1);

	h = INVALID_HANDLE_VALUE;
	pn = comPort;		/* Current setting */
	do {	/* Find next available port */
		pn += dir;
		if (pn < 1 || pn > 99) {
			MessageBeep(MB_OK);
			break;
		}
		sprintf(str, "\\\\.\\COM%u", pn);
		h = CreateFile(str, GENERIC_READ|GENERIC_WRITE, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	} while (h == INVALID_HANDLE_VALUE);

	if (h != INVALID_HANDLE_VALUE) {
		if (init_port(h)) {
			if (hComm != INVALID_HANDLE_VALUE)
				CloseHandle(hComm);
			hComm = h;
			comPort = pn;
			set_title();
		} else {
			CloseHandle(h);
			MessageBeep(MB_OK);
		}
	}

	RcvrCmd = 0;
}


void next_view(void) {
    switch (view) {
        case VIEW_TTY:
            view        = VIEW_HEX;
            comViewName = viewNameHex;
            break;

        case VIEW_HEX:
            view        = VIEW_TTY;
            comViewName = viewNameTty;
            break;
    }

    set_title();
}



void change_parity (void)
{
	if (hComm == INVALID_HANDLE_VALUE || AutoXmit) {
		MessageBeep(MB_OK);
		return;
	}

	switch (comParity) {
		case NOPARITY:
			comParity = ODDPARITY;
			break;
		case ODDPARITY:
			comParity = EVENPARITY;
			break;
		default:
			comParity = NOPARITY;
			break;
	}
	comDcb.Parity = comParity;
	RcvrCmd = RCVR_HALT;	/* Get background thread away from comm function */
	while (RcvrCmd != RCVR_HALTING) Sleep(1);

	if (SetCommState(hComm, &comDcb)) {
		set_title();
	} else {
		MessageBeep(MB_OK);
	}
	RcvrCmd = 0;
}




void change_bps (int cmd)
{
	const int bpstbl[] = {300, 1200, 2400, 4800, 9600, 19200, 38400, 57600, 115200, 230400, 460800, 921600};
	int i;


	if (hComm == INVALID_HANDLE_VALUE || AutoXmit) {
		MessageBeep(MB_OK);
		return;
	}

	for (i = sizeof bpstbl / sizeof bpstbl[0] - 1; i && comBps < bpstbl[i]; i--) ;	/* Snap to nearest lower rate */
	switch (cmd) {
	case KCMD_BPSUP:	/* Next upper rate */
		if (i < sizeof bpstbl / sizeof bpstbl[0] - 1) {
			i++;
		} else {
			MessageBeep(MB_OK);
		}
		break;
	case KCMD_BPSDN:	/* Next lower rate */
		if (i) {
			i--;
		} else {
			MessageBeep(MB_OK);
		}
		break;
	default:
		return;
	}
	comDcb.BaudRate = bpstbl[i];

	RcvrCmd = RCVR_HALT;	/* Get background thread away from comm function */
	while (RcvrCmd != RCVR_HALTING) Sleep(1);

	if (SetCommState(hComm, &comDcb)) {
		comBps = bpstbl[i];
		set_title();
	} else {
		comDcb.BaudRate = comBps;
		MessageBeep(MB_OK);
	}

	RcvrCmd = 0;
}




int get_column (char **src, char *dst, int sz)
{
	BYTE c;
	int ret = 0;

	while (1) {
		c = **src;
		if (c < ' ') break;
		(*src)++;
		if (c == ',') break;
		if (sz > 1) {
			*dst++ = c;
			sz--;
		} else {
			ret = 1;
		}
	}
	*dst++ = 0;

	return ret;
}




BOOL load_cfg(int argc, char **argv)
{
	char *cp, *sp, *cmdlst[20], cmdbuff[256], str[10];
	char inifile[_MAX_PATH], *dmy;
	int cmd, n;
	FILE *fp;


	GetCurrentDirectory(sizeof sLogFile, sLogFile);
	cp = sLogFile + strlen(sLogFile);
	if (strchr(sLogFile, '\\') != (cp-1))
		strcpy(cp, "\\*.txt");

	cmd = 0; cp = cmdbuff;

	/* Import ini file as command line parameters */

	if ((fp = fopen(INIFILE, "rt")) == NULL) {
		if (SearchPath(NULL, INIFILE, NULL, sizeof inifile, inifile, &dmy))
			fp = fopen(inifile, "rt");
	}
	if (fp != NULL) {
		while(fgets(cp, cmdbuff + sizeof cmdbuff - cp, fp) != NULL) {
			if (cmd >= (sizeof cmdlst / sizeof cmdlst[0] - 1)) break;
			if (*cp <= ' ') break;
			if ((sp = strstr(cp, "\n")) != NULL) *sp = '\0';
			cmdlst[cmd++] = cp; cp += strlen(cp) + 1;
		}
		fclose(fp);
	}

	/* Get command line parameters */
	while(--argc && (cmd < (sizeof cmdlst / sizeof cmdlst[0] - 1)))
		cmdlst[cmd++] = *++argv;
	cmdlst[cmd] = NULL;

	/* Analyze command line parameters... */
	if (cmdlst[0] == NULL) return FALSE;	/* No parameter */
	for (cmd = 0; cmdlst[cmd] != NULL; cmd++) {
		cp = cmdlst[cmd];
		if (strstr(cp, "port=") == cp) {
			cp += 5;
			get_column(&cp, str, sizeof str);
			n = atoi(str);
			if (n) comPort = n;
			get_column(&cp, str, sizeof str);
			if (str[0]) {
				if (tolower(str[0]) == 'n') comParity = NOPARITY;
				if (tolower(str[0]) == 'o') comParity = ODDPARITY;
				if (tolower(str[0]) == 'e') comParity = EVENPARITY;
				if (str[1]) {
					if (str[1] == '8') comData = 8;
					if (str[1] == '7') comData = 7;
					if (str[2]) {
						if (str[2] == '1') comStop = ONESTOPBIT;
						if (str[2] == '2') comStop = TWOSTOPBITS;
					}
				}
			}
			get_column(&cp, str, sizeof str);
			n = atoi(str);
			if (n) comBps = n;
			continue;
		}
		if (strstr(cp, "help=") == cp) {
			comHelp = atoi(cp+5);
			continue;
		}
		if (strstr(cp, "bps=") == cp) {
			n = atoi(cp+4);
			if (n) comBps = n;
			continue;
		}
		if (strstr(cp, "pol=") == cp) {
			n = atoi(cp+4);
			if (n) comPol = n;
			continue;
		}
		if (strstr(cp, "flow=") == cp) {
			if (strstr(cp+5, "on") == cp+5) comCtsflow = TRUE;
			if (strstr(cp+5, "off") == cp+5) comCtsflow = FALSE;
			continue;
		}
		if (strstr(cp, "log=") == cp) {
			hLog = CreateFile(cp+4, GENERIC_WRITE, 0, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
			if (hLog != INVALID_HANDLE_VALUE) {
				SetFilePointer(hLog, 0, NULL, FILE_END);
				GetFileTitle(cp+4, sLogFileTitle, sizeof sLogFileTitle);
			}
			continue;
		}
		return FALSE;
	} /* for */

	return TRUE;
}




DWORD WINAPI RcvrThread (LPVOID parms)
{
	BYTE dbuff[17], c, f, b;
	BYTE str[24];
	DWORD d, sc, i, nrc;


	for (;;) {
		if (RcvrCmd == RCVR_EXIT) break;		/* Exit thread */
		if (RcvrCmd == RCVR_HALT) RcvrCmd = RCVR_HALTING;	/* Get away from comm function */
		if (RcvrCmd == RCVR_HALTING || hComm == INVALID_HANDLE_VALUE) {
			Sleep(1); continue;
		}

		if (RcvrCmd == KCMD_HANG) {
			RcvrCmd = 0;
			EscapeCommFunction(hComm, (comPol & 1) ? SETDTR : CLRDTR);
			Sleep(TMR_HANG);
			EscapeCommFunction(hComm, (comPol & 1) ? CLRDTR : SETDTR);
		}
		if (RcvrCmd == KCMD_BREAK) {
			RcvrCmd = 0;
			if (!(comPol & 4)) {
				EscapeCommFunction(hComm, SETBREAK);
				Sleep(TMR_BREAK);
				EscapeCommFunction(hComm, CLRBREAK);
			}
		}
		if (nTxb) {
			i = nTxb;
			if (!(comPol & 4)) WriteFile(hComm, Txb, nTxb, &i, NULL);
			nTxb -= i;
			if (nTxb)
				memmove(Txb, Txb + i, nTxb);
		}
		nrc = 0;
		ReadFile(hComm, Rxb, sizeof Rxb, &nrc, NULL);
		if (nrc == 0) continue;

		Rxc = Rxb[0];
		if (AutoXmit == KCMD_XMODEM) continue;

		if (view == 1) {	/* HEX dump mode */
			for (i = 0; i < nrc; i++) {
				c = Rxb[i];
				dbuff[DumpAddr & 15] = (c >= 0x20 && c <= 0x7E) ? c : '.';
				switch(DumpAddr & 15) {
					case 0:
						sprintf(str, "%08X: %02X", DumpAddr, c);
						sc = 12;
						break;
					case 8:
						sprintf(str, "-%02X", c);
						sc = 3;
						break;
					case 15:
						dbuff[16] = 0;
						sprintf(str, " %02X  %s\r\n", c, dbuff);
						sc = 23;
						break;
					default:
						sprintf(str, " %02X", c);
						sc = 3;
				}
				WriteConsole(hScreen, str, sc, &d, NULL);
				if (hLog != INVALID_HANDLE_VALUE)
					WriteFile(hLog, str, sc, &d, NULL);
				DumpAddr++;
			}
		} else {	/* TTY mode */
			/* Put current time-string at the begin of a new line in Rxb if fTime is active */
			if (fTime) {
				for (i = 0; i < nrc; i++) {
					c = Rxb[i];
					if (!lineBegin) {
						if (c == 0x0a || c == 0x0d) {
							lineBegin = 1;
						}
					} else {
						if (c != 0x0a && c != 0x0d) {
							SYSTEMTIME st;
							char dateStr[50], len;
							lineBegin = 0;
							GetLocalTime(&st);
							snprintf(dateStr, 50, "%02d.%02d %02d:%02d:%02d,%03d> ", st.wDay,
                                     st.wMonth, st.wHour, st.wMinute, st.wSecond, st.wMilliseconds);
							len = strlen(dateStr);
							/* prevent buffer overflow */
							if ((nrc + len) < 1024) {
								memmove(&Rxb[i+len], &Rxb[i], nrc - i);
								memmove(&Rxb[i], dateStr, len);
								nrc += len;
							}
						}
					}
				}
			}

			if (hLog != INVALID_HANDLE_VALUE)
				WriteFile(hLog, Rxb, nrc, &d, NULL);
			i = b = 0;
			do {
				sc = 0;
				c = Rxb[i];
				if (c < 0x20 && c != '\r' && c != '\n' && c != '\b' && c != '\t') {
					f = 1;
					do {
						Rxb[i + sc] += '@';
						sc++; nrc--;
						c = Rxb[i + sc];
						if (c == '\a') b = 1;
					} while (nrc && c < 0x20 && c != '\r' && c != '\n' && c != '\b' && c != '\t');
				} else {
					f = 0;
					do {
						sc++; nrc--;
						c = Rxb[i + sc];
					} while (nrc && (c >= 0x20 || c == '\r' || c == '\n' || c == '\b' || c == '\t'));
				}
				if (f) SetConsoleTextAttribute(hScreen, (WORD)((ScreenInfo.wAttributes & (BACKGROUND_RED | BACKGROUND_GREEN | BACKGROUND_BLUE | BACKGROUND_INTENSITY)) | FOREGROUND_RED | FOREGROUND_GREEN));	/* Yellow */
				WriteConsole(hScreen, &Rxb[i], sc, &d, NULL);
				if (f) SetConsoleTextAttribute(hScreen, ScreenInfo.wAttributes);	/* Normal */
				i += sc;
			} while (nrc);
			if (b) MessageBeep(MB_OK);
		}
	}

	RcvrCmd = 0;
	return 0;
}




int proc_key (void)
{
	int c, kcmd, nchr, nc;
	DWORD n;
	INPUT_RECORD ir;
	static BYTE xbuf[136];
	static int xsize, val, dc;


	if (AutoXmit && !nTxb) {
		if (AutoXmit == KCMD_BINARY) {
			ReadFile(hTransmit, Txb, sizeof Txb, &nc, NULL);
			FilePtr += nc;
			if (nc) {
				nTxb = nc;
			} else {
				stop_transmisson();
			}
			set_title();
		} else {
			c = Rxc;
			if (c) {
				Rxc = 0;
				if (Xmode == 0) {
					xbuf[0] = SOH; xbuf[1] = 0;
					switch (c) {
					case NAK:
						Xmode = 1;
						c = ACK;
						break;
					case 'C':
						Xmode = 2;
						c = ACK;
						break;
					default :
						c = 0;
					}
				}
				if (c == NAK || c == ACK) {
					if (c == ACK) {
						if (xbuf[0] == EOT) {
							stop_transmisson();
							xsize = 0;
						} else {
							ReadFile(hTransmit, &xbuf[3], 128, &nc, NULL);
							FilePtr += nc;
							if (nc) {
								xbuf[1]++; xbuf[2] = ~xbuf[1];
								memset(&xbuf[3 + nc], 0, 128 - nc);
								xsize = 3 + create_sum(&xbuf[3], 128, Xmode);
							} else {
								xbuf[0] = EOT;
								xsize = 1;
							}
						}
					}
					memcpy(Txb, xbuf, xsize);
					nTxb = xsize;
					set_title();
				}
			}
		}
	}

	kcmd = 0; nchr = 0;
	for (;;) {
		PeekConsoleInput(hKey, &ir, 1, &n);
		if (n == 0) break;
		ReadConsoleInput(hKey, &ir, 1, &n);
		if (ir.EventType != KEY_EVENT) break;
		if (ir.Event.KeyEvent.bKeyDown == FALSE) {
			if (ir.Event.KeyEvent.wVirtualKeyCode == VK_MENU) {
				if (!AutoXmit && dc && !nTxb)
					Txb[nchr++] = (BYTE)val;
				dc = val = 0;
			}
			break;
		}
		c = ir.Event.KeyEvent.wVirtualKeyCode;
		if (AutoXmit) {
			if (c == VK_ESCAPE)
				kcmd = KCMD_STOP;
			continue;
		}
		if (ir.Event.KeyEvent.dwControlKeyState & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED) ) {
			if (c >= VK_NUMPAD0 && c <= VK_NUMPAD9) {
				val = (val % 100) * 10 + (c - VK_NUMPAD0);
				dc = 1;
			} else {
				dc = val = 0;
				switch (c) {
					case 'X':
						kcmd = KCMD_EXIT;
						break;
					case 'L':
						kcmd = KCMD_SWLOG;
						break;
					case 'G': /* MBo */
						kcmd = KCMD_SWLOG2;
						break;
					case 'Z': /* MBo */
						kcmd = KCMD_TIME;
						break;
					case 'P': /* MBo */
						kcmd = KCMD_PARITY;
						break;
					case 'B':
						kcmd = KCMD_BREAK;
						break;
					case 'H':
						kcmd = KCMD_HANG;
						break;
					case 'V':
						kcmd = KCMD_VIEW;
						break;
					case 'T':
						kcmd = KCMD_BINARY;
						break;
					case 'Y':
						kcmd = KCMD_XMODEM;
						break;
					case VK_UP:
						kcmd = KCMD_BPSUP;
						break;
					case VK_DOWN:
						kcmd = KCMD_BPSDN;
						break;
					case VK_RIGHT:
						kcmd = KCMD_PORTUP;
						break;
					case VK_LEFT:
						kcmd = KCMD_PORTDN;
						break;
				}
			}
		} else {
			dc = val = 0;
			c = ir.Event.KeyEvent.uChar.AsciiChar;
			if (c) {
				for (n = 500; n && nTxb; n--) Sleep(1);
				if (n) Txb[nchr++] = c;
			}
		}
	}

	if (nchr) nTxb = nchr;

	return kcmd;
}




int main (int argc, char **argv)
{
	DWORD n;
	int cmd;


	/* Configure program */
	if (!load_cfg(argc, argv)) {
		printf(Usage1);
		printf(Usage2);
		return 3;
	}

	/* Get console handles */
	if ((hScreen = GetStdHandle(STD_OUTPUT_HANDLE)) == INVALID_HANDLE_VALUE)
		return 2;
	if ((hKey = GetStdHandle(STD_INPUT_HANDLE)) == INVALID_HANDLE_VALUE)
		return 2;
	GetConsoleScreenBufferInfo(hScreen, &ScreenInfo);
	SetConsoleMode(hKey, 0);

	/* Start backgrond thread */
	hRcvrThread = CreateThread(NULL, 0, RcvrThread, 0, 0, &n);
	if (hRcvrThread == INVALID_HANDLE_VALUE) {
		CloseHandle(hComm);
		return 2;
	}

	/* Open COM port first */
	open_port(OPEN_FIRST);
	if (hComm == INVALID_HANDLE_VALUE) {
		printf("COM%u is not available.\nChange port with Alt-Left/Right.\n", comPort);
		MessageBeep(MB_OK);
	}
	set_title();

	GetCurrentDirectory(sizeof sOpenFile, sOpenFile);
	strcat(sOpenFile, (sOpenFile[strlen(sOpenFile) - 1] == '\\') ? "*.*" : "\\*.*");

	if (comHelp) printf(Usage2);

	for (;;) {
		do {
			Sleep(1);
			cmd = proc_key();
		} while (!cmd);

		switch (cmd) {
		case KCMD_EXIT:
			RcvrCmd = RCVR_EXIT;	/* Stop background thread */
			for (n = 1000; n && RcvrCmd; n--) Sleep(1);
			if (n && hComm != INVALID_HANDLE_VALUE)
				CloseHandle(hComm);
			if (hTransmit != INVALID_HANDLE_VALUE)
				CloseHandle(hTransmit);
			if (hLog != INVALID_HANDLE_VALUE)
				CloseHandle(hLog);
			return 0;

		case KCMD_SWLOG:
			switch_logging(0); /* MO */
			break;
		case KCMD_SWLOG2: /* MO */
			switch_logging(1);
			break;

		case KCMD_VIEW:
		    next_view();
            DumpAddr = 0;
            WriteConsole(hScreen, "\r\n", 2, &n, NULL);
            if (hLog != INVALID_HANDLE_VALUE)
                WriteFile(hLog, "\r\n", 2, &n, NULL);
			break;

		case KCMD_TIME: /* MO */
			fTime = ~fTime;
			break;

		case KCMD_BINARY:
		case KCMD_XMODEM:
			start_transmisson(cmd);
			break;

		case KCMD_HANG:
		case KCMD_BREAK:
			RcvrCmd = cmd;
			break;

		case KCMD_STOP:
			stop_transmisson();
			break;

		case KCMD_BPSUP:
		case KCMD_BPSDN:
			change_bps(cmd);
			break;

		case KCMD_PARITY:
			change_parity();
			break;

		case KCMD_PORTUP:
		case KCMD_PORTDN:
			open_port(cmd);
			break;
		}
	}

}

