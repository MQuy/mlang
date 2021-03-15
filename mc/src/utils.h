#ifndef UTILS_H
#define UTILS_H 1

#include <cassert>
#include <iostream>

struct EnumClassHash
{
	template <typename T>
	std::size_t operator()(T t) const
	{
		return static_cast<std::size_t>(t);
	}
};

#define assert_not_reached() ({ std::cout << __FILE__ << __LINE__ << __func__; asm volatile("ud2"); })

#endif
