#include <stdio.h>
#include <stdbool.h>
int main ()
{
int a = 10;
double b = 10.5;
printf("%lu\n", sizeof(b));
float c = 10.50;
printf("%lu\n", sizeof(c));
char d = 'a';
char e[] = "char array"; // aka a string
bool f = false;

int slices = 17;
int people = 2;
double slicesPerPerson = (double) slices / people;
printf("%lf\n", slicesPerPerson);
double 
return 0;
}
