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

[[noreturn]] void __assertion_failed();

#define assert_not_reached()                           \
	do                                                 \
	{                                                  \
		std::cout << __FILE__ << __LINE__ << __func__; \
		__assertion_failed();                          \
	} while (0)

#define assert_not_implemented()                                                    \
	do                                                                              \
	{                                                                               \
		std::cout << __FILE__ << __LINE__ << __func__ << " is not implemented yet"; \
		__assertion_failed();                                                       \
	} while (0)

#endif
