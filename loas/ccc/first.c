#include <stdio.h>

int main()
{
	printf("Hello World\n");

	int x = 50;
	int y; //declaration
	y = 10; //initialitation
	
	printf("%s World\n", "Hello"); //%s for string
	printf("The value of x is %d\n", x); //%d for numbers
	printf("x is %d, y is %d\n",x, y);

	printf("Give me a radius: ");
	int radius;
	scanf("%d",  &radius); //addres of operatior (pointer)

	printf("You chose the value %d\n", radius);
	char name[20]; //char array 20 characters. '\0' takes one spot
	scanf("%19s", name);
	printf("Your name: %s\n", name);
	return 0;
}
