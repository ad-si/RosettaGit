+++
title = "Brownian tree/C++ animated"
description = ""
date = 2014-04-15T20:27:53Z
aliases = []
[extra]
id = 13333
[taxonomies]
categories = []
tags = []
+++

[[File:brownianTreeAnim_cpp.png|300px]]

The green dots you can see in this picture are what I call movers. They are particles that still do not belong to the tree.

```cpp

#include <windows.h>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
enum states { SEED, GROWING, MOVING, REST };
enum treeStates { NONE, MOVER, TREE };
const int MAX_SIDE = 500, MAX_MOVERS = 511, MAX_CELLS = 15137;

//--------------------------------------------------------------------------------------------------
struct point
{
    point() : x(0), y(0)                 { }
    point( int a, int b ) : x(a), y(b)   { }
    void set( int a, int b ) { x = a; y = b; }
    int x, y;
};
//--------------------------------------------------------------------------------------------------
struct movers
{
    point pos;
    bool moving;
    movers() : moving( false ){}
};
//--------------------------------------------------------------------------------------------------
class myBitmap
{
public:
    myBitmap() : pen( NULL ) {}
    ~myBitmap()
    {
	DeleteObject( pen );
	DeleteDC( hdc );
	DeleteObject( bmp );
    }

    bool create( int w, int h )
    {
	BITMAPINFO	bi;
	ZeroMemory( &bi, sizeof( bi ) );

	bi.bmiHeader.biSize	   = sizeof( bi.bmiHeader );
	bi.bmiHeader.biBitCount	   = sizeof( DWORD ) * 8;
	bi.bmiHeader.biCompression = BI_RGB;
	bi.bmiHeader.biPlanes	   = 1;
	bi.bmiHeader.biWidth	   =  w;
	bi.bmiHeader.biHeight	   = -h;

	HDC dc = GetDC( GetConsoleWindow() );
	bmp = CreateDIBSection( dc, &bi, DIB_RGB_COLORS, &pBits, NULL, 0 );
	if( !bmp ) return false;

	hdc = CreateCompatibleDC( dc );
	SelectObject( hdc, bmp );
	ReleaseDC( GetConsoleWindow(), dc ); 

	width = w; height = h;

	return true;
    }

    void clear()
    {
	ZeroMemory( pBits, width * height * sizeof( DWORD ) );
    }

    void setPenColor( DWORD clr )
    {
	if( pen ) DeleteObject( pen );
	pen = CreatePen( PS_SOLID, 1, clr );
	SelectObject( hdc, pen );
    }

    void saveBitmap( string path )
    {
	BITMAPFILEHEADER fileheader;
	BITMAPINFO	 infoheader;
	BITMAP		 bitmap;
	DWORD		 wb;

	GetObject( bmp, sizeof( bitmap ), &bitmap );

	DWORD* dwpBits = new DWORD[bitmap.bmWidth * bitmap.bmHeight];
	ZeroMemory( dwpBits, bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD ) );
	ZeroMemory( &infoheader, sizeof( BITMAPINFO ) );
	ZeroMemory( &fileheader, sizeof( BITMAPFILEHEADER ) );

	infoheader.bmiHeader.biBitCount = sizeof( DWORD ) * 8;
	infoheader.bmiHeader.biCompression = BI_RGB;
	infoheader.bmiHeader.biPlanes = 1;
	infoheader.bmiHeader.biSize = sizeof( infoheader.bmiHeader );
	infoheader.bmiHeader.biHeight = bitmap.bmHeight;
	infoheader.bmiHeader.biWidth = bitmap.bmWidth;
	infoheader.bmiHeader.biSizeImage = bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD );

	fileheader.bfType    = 0x4D42;
	fileheader.bfOffBits = sizeof( infoheader.bmiHeader ) + sizeof( BITMAPFILEHEADER );
	fileheader.bfSize    = fileheader.bfOffBits + infoheader.bmiHeader.biSizeImage;

	GetDIBits( hdc, bmp, 0, height, ( LPVOID )dwpBits, &infoheader, DIB_RGB_COLORS );

	HANDLE file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
	WriteFile( file, &fileheader, sizeof( BITMAPFILEHEADER ), &wb, NULL );
	WriteFile( file, &infoheader.bmiHeader, sizeof( infoheader.bmiHeader ), &wb, NULL );
	WriteFile( file, dwpBits, bitmap.bmWidth * bitmap.bmHeight * 4, &wb, NULL );
	CloseHandle( file );

	delete [] dwpBits;
    }

    HDC getDC() const     { return hdc; }
    int getWidth() const  { return width; }
    int getHeight() const { return height; }

private:
    HBITMAP bmp;
    HDC	    hdc;
    HPEN    pen;
    void	*pBits;
    int	    width, height;
};
//--------------------------------------------------------------------------------------------------
class brownianTree
{
public:
    brownianTree()         
    { 
	_bmp.create( MAX_SIDE, MAX_SIDE );
	init(); 
    }

    void init()
    {
	_state = SEED;
	_cellCount = 0;
	for( int x = 0; x < 20; x++ )
	    _color[x] = RGB( rand() % 150 + 75, rand() % 200 + 50, rand() % 180 + 45 );
	ZeroMemory( _grid, sizeof( _grid ) );
    }

    void mainLoop()
    {
	switch( _state )
	{
	    case REST: break;
	    case SEED: doSeed(); break;
	    case GROWING: startMovers(); break;
	    case MOVING: moveMovers();
	}
	drawGrid();
    }

    void setHwnd( HWND h ) { _hWnd = h; }

private:
    void drawGrid()
    {
	_bmp.clear();

	for( int y = 0; y < MAX_SIDE; y++ )
	    for( int x = 0; x < MAX_SIDE; x++ )
	    {
		BYTE g = _grid[x][y];
		if( g != NONE )
		{
		    DWORD clr = g > MOVER ? _color[g - 2] : RGB( 0, 255, 0 );
		    SetPixel( _bmp.getDC(), x, y, clr );
		}
	    }

	    HDC dc = GetDC( _hWnd );
	    BitBlt( dc, 0, 0, MAX_SIDE, MAX_SIDE, _bmp.getDC(), 0, 0, SRCCOPY );
	    ReleaseDC( _hWnd, dc );
    }

    void doSeed()
    {
	int x = MAX_SIDE - MAX_SIDE / 2, y = MAX_SIDE / 4;
	_grid[rand() % x + y][rand() % x + y] = TREE;
	_cellCount++;
	_state = GROWING;
    }

    void addMover( movers* m )
    {
	m->moving = true;
	int x = MAX_SIDE - MAX_SIDE / 2, y = MAX_SIDE / 4, a, b;
	do
	{
	    a = rand() % x + y; b = rand() % x + y;
	} while ( _grid[a][b] != NONE )

	m->pos.set( a, b );
	_grid[a][b] = MOVER;
    }

    void startMovers()
    {
	for( int c = 0; c < MAX_MOVERS; c++ )
	{
	    movers* m = &_movers[c];
	    addMover( m );
	}
	_state = MOVING;
    }

    void addToTree( movers* m )
    {
	m->moving = false;
	_grid[m->pos.x][m->pos.y] = ( BYTE )GetTickCount() % 20;
	++_cellCount;
    }

    bool moveIt( movers* m )
    {
	point f[8]; int ff = 0;
	for( int y = -1; y < 2; y++ )
	{
	    for( int x = -1; x < 2; x++ )
	    {
		if( !x && !y ) continue;
		int a = m->pos.x + x, b = m->pos.y + y;
		if( a < 0 || b < 0 || a >= MAX_SIDE || b >= MAX_SIDE )
		{
		    addToTree( m );
		    return true;
		}
				
		BYTE g = _grid[a][b];
		if( g > MOVER )
		{
		    addToTree( m );
		    return true;
		}
		else if( g == NONE ) f[ff++].set( a, b );
	    }
	}

	if( ff < 1 ) return false;
	_grid[m->pos.x][m->pos.y] = NONE;
	m->pos = f[rand() % ff];
	_grid[m->pos.x][m->pos.y] = MOVER;

	return false;
    }

    void moveMovers()
    {
	bool found = false;
	for( int m = 0; m < MAX_MOVERS; m++ )
	{
	    movers* mm = &_movers[m];
	    if( !mm->moving ) continue;
	    found = true;
	    if( moveIt( mm ) && _cellCount < MAX_CELLS ) addMover( mm );
	}

	if( !found ) _state = REST;
    }

    HWND     _hWnd;
    states   _state;
    BYTE     _grid[MAX_SIDE][MAX_SIDE];
    myBitmap _bmp;
    int      _cellCount;
    movers   _movers[MAX_MOVERS];
    DWORD    _color[20];
};
//--------------------------------------------------------------------------------------------------
LRESULT CALLBACK WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
    switch( msg )
    {
	case WM_DESTROY: PostQuitMessage( 0 ); break;
	default:
	    return DefWindowProc( hWnd, msg, wParam, lParam );
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------
HWND InitAll( HINSTANCE hInstance )
{
    WNDCLASSEX wcex;
    ZeroMemory( &wcex, sizeof( wcex ) );

    wcex.cbSize	       = sizeof( WNDCLASSEX );
    wcex.style	       = CS_HREDRAW | CS_VREDRAW;
    wcex.lpfnWndProc   = WndProc;
    wcex.hInstance     = hInstance;
    wcex.hCursor       = LoadCursor( NULL, IDC_ARROW );
    wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
    wcex.lpszClassName = "_BROWNIAN_";

    RegisterClassEx( &wcex ); 
    return CreateWindow( "_BROWNIAN_", ".: Brownian Tree -- PJorente :.", WS_SYSMENU, CW_USEDEFAULT, 0, MAX_SIDE, MAX_SIDE, NULL, NULL, hInstance, NULL );
}
//--------------------------------------------------------------------------------------------------
int APIENTRY _tWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow )
{
    srand( GetTickCount() );

    HWND hwnd = InitAll( hInstance );
    if( !hwnd ) return -1;

    RECT rc = { 0, 0, MAX_SIDE, MAX_SIDE };

    AdjustWindowRectEx( &rc, WS_SYSMENU | WS_CAPTION, FALSE, 0 );
    int w = rc.right  - rc.left, 
	h = rc.bottom - rc.top;

    int posX = ( GetSystemMetrics( SM_CXSCREEN ) >> 1 ) - ( w >> 1 ),
        posY = ( GetSystemMetrics( SM_CYSCREEN ) >> 1 ) - ( h >> 1 );

    SetWindowPos( hwnd, HWND_TOP, posX, posY, w, h, SWP_NOZORDER );
    ShowWindow( hwnd, nCmdShow );
    UpdateWindow( hwnd );

    brownianTree tree;
    tree.setHwnd( hwnd );

    MSG msg;
    ZeroMemory( &msg, sizeof( msg ) );
    while( msg.message != WM_QUIT )
    {
	if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) != 0 )
	{
	    TranslateMessage( &msg );
	    DispatchMessage( &msg );
	}
	else
	{
	    tree.mainLoop();
	}
    }
    return UnregisterClass( "_BROWNIAN_", hInstance );
}
//--------------------------------------------------------------------------------------------------

```

