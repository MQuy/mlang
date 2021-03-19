#ifndef UTILS_H
#define UTILS_H 1

#include <cassert>
#include <iostream>

#define assert_not_reached()                           \
	do                                                 \
	{                                                  \
		std::cout << __FILE__ << __LINE__ << __func__; \
		assert(false);                                 \
	} while (0);

#endif
