+++
title = "Bitmap/C++"
description = ""
date = 2011-05-13T10:30:15Z
aliases = []
[extra]
id = 5305
[taxonomies]
categories = []
tags = []
+++

{{collection|Basic bitmap storage}}


```cpp
#include <cstddef>
#include <stdexcept>

// CBitmap as found below is a class that represents bitmap images
// in main memory, alternatively the images could be stored in
// memory in a graphics system (OpenGL, DirectX, ...), but this
// would not be generic, nor simple.

class CBitmap
{
private:
        enum { red_index, green_index, blue_index, num_channels };

private:
        char* m_pImageData;
        unsigned int m_width;
        unsigned int m_height;

public:
        // If allocation fails, "new" will throw a std::bad_alloc exception.
        // For automatic variables, the result is that all accesses to the
        // class' members are skipped, by leaving the scope where the object
        // is created, and visible.
        // For dynamic variables, created on the heap, this will not result
        // in a memory leak, because the memory allocated for the CBitmap
        // is freed as a result of the exception in this case.  It is the
        // responsability of the client not to access the class' members
        // after such a failed allocation.
        // The result of this is that the m_pImageData member variable
        // will never be 0 in a legal access through the class' member
        // functions, and thus we can be certain of the invariant (m_pImageData != 0).
        CBitmap(unsigned int width, unsigned int height):
                m_pImageData(new char[num_channels * width * height]),
                m_width(width),
                m_height(height)
        {
        }

        // The presence of this copy constructor enables pass-by-value,
        // which is strongly discouraged, due to large amount of work
        // involved in copying.  Use pass-by-reference to avoid the copy.
        CBitmap(CBitmap const &original):
                m_pImageData(new char[num_channels * original.m_width * original.m_height]),
                m_width(original.m_width),
                m_height(original.m_height)
        {
                CopyImageDataFrom(original.m_pImageData);
        }

        ~CBitmap()
        {
                delete [] m_pImageData;
        }

public:
        // An assignment operator is defined with copy-semantics. When an
        // allocation error occurs, an exception is thrown (which should
        // be caught by the client, and the object destructed) and the
        // original data is preserved, to satisfy the invariant (m_pImageData != 0).

        // post-condition: this bitmap becomes a uniform copy of the original.
        // exception: failed allocation will cause the image data to be unchanged.
        CBitmap& operator=(CBitmap const &original)
        {
                if ( this == &original )
                {
                        return *this;
                }

                try
                {
                        char *new_image_data = new char[num_channels * original.m_width * original.m_height];
                        delete [] m_pImageData;
                        m_pImageData = new_image_data;
                        m_width = original.m_width;
                        m_height = original.m_height;
                        CopyImageDataFrom(original.m_pImageData);
                }
                catch ( ... )
                {
                        throw std::runtime_error( "assignment failed, original data conserved" );
                }

                return *this;
        }


public:
        bool SetPixel(unsigned int x, unsigned int y, char R, char G, char B)
        {
                if ( ! IsWithinBitmap(x, y) )
                {
                        return false;
                }

                unsigned int pixel_index = ImageCoordinateToPixelIndex(x, y);
                SetColorValueAtIndex( pixel_index, R, G, B );

                return true;
        }

        bool GetPixel(unsigned int x, unsigned int y, char& R, char& G, char& B)
        {
                if ( ! IsWithinBitmap(x, y) )
                {
                        return false;
                }

                unsigned int pixel_index = ImageCoordinateToPixelIndex(x, y);
                GetColorValueAtIndex( pixel_index, R, G, B );

                return true;
        }

        void Fill(char R, char G, char B)
        {
                for(unsigned int pixel_index = 0;
                                pixel_index < m_height * m_width * num_channels;
                                pixel_index += num_channels)
                        SetColorValueAtIndex( pixel_index, R, G, B );
        }

private:
        // An alternative to status flags is the use of exceptions.
        bool IsWithinBitmap(unsigned int x, unsigned int y)
        {
                return y < m_height && x < m_width;
        }

        unsigned int ImageCoordinateToPixelIndex(unsigned int x, unsigned int y)
        {
                return (y * m_width + x) * num_channels;
        }

        void SetColorValueAtIndex(unsigned int pixel_index, char R, char G, char B)
        {
                m_pImageData[pixel_index + red_index] = R;
                m_pImageData[pixel_index + green_index] = G;
                m_pImageData[pixel_index + blue_index] = B;
        }

        void GetColorValueAtIndex(unsigned int pixel_index, char R, char G, char B)
        {
                R = m_pImageData[pixel_index + red_index];
                G = m_pImageData[pixel_index + green_index];
                B = m_pImageData[pixel_index + blue_index];
        }

        void CopyImageDataFrom(char *source)
        {
                // An alternative implementation using memcpy would be more efficient
                // on almost all platforms.
                for (unsigned int i = 0; i < num_channels * m_width * m_height; ++i)
                        m_pImageData[i] = source[i];
        }

};
```

