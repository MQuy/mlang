#ifndef CONFIG_H
#define CONFIG_H 1

#include <string>
#include <vector>

struct Config
{
	Config(std::vector<std::string> include_paths, std::string dir_path)
		: include_paths(include_paths)
		, current_path(dir_path)
	{
	}

	std::vector<std::string> include_paths;
	std::string current_path;
};

#endif
