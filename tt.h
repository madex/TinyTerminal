/* interface for extension
 *
 * (c) 2015 Martin Boekhoff
 */

#ifndef TT_H
#define TT_H

typedef void (*extKeyHandler_t)(int key);
typedef void (*extRecv_t)(char recvByte);

typedef struct {
	char name[20];
	int  comDcb;
	int  comBps;
	int  comData;
	int  comStop;
	int  comParity;
	BOOL comCtsflow;
	int  comPol;
	extKeyHandler_t eventHandler;
	extRecv_t reveiveFunc;
} extension_data_t;

char registerExtension(extension_data_t extData);



#endif
