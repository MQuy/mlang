#include <assert.h>
extern int printf(const char*, ...);

struct condstruct
{
	int i;
};

static int getme(struct condstruct* s, int i)
{
	int i1 = (i != 0 ? 0 : s)->i;
	int i2 = (i == 0 ? s : 0)->i;
	int i3 = (i != 0 ? (void*)0 : s)->i;
	int i4 = (i == 0 ? s : (void*)0)->i;
	return i1 + i2 + i3 + i4;
}

int main()
{
	int Count;

	for (Count = 0; Count < 10; Count++)
	{
		printf("%d\n", (Count < 5) ? (Count * Count) : (Count * 3));
	}

	{
		int c = 0;
		struct stru
		{
			int x;
		} a = {'A'}, b = {'B'};
		const struct stru2
		{
			int x;
		} d = {'D'};
		c = 1;
		struct condstruct cs = {38};
		printf("%d\n", getme(&cs, 0));

		// the following lines contain type mismatch errors in every ternary expression
		//printf("comparing double with pointer : size = %d\n", sizeof(0 ? &c : 0.0));
		//printf("'%c' <> '%c'\n", (0 ? a : d).x, (1 ? a : d).x);
		//0 ? a : 0.0;
	}

	return 0;
}

/* vim: set expandtab ts=4 sw=3 sts=3 tw=80 :*/
