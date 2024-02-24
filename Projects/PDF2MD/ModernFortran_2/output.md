26
Getting started:
Minimal working app
In this chapter, we’ll implement the minimal working version of the tsunami simu-
lator. For simplicity, we’ll start by simulating the movement of water in space due to
background flow, without changing its shape. This problem is sufficiently complex
to introduce basic elements of Fortran: numeric data types, declaration, arithmetic
expressions and assignment, and control flow. Once we successfully simulate the
movement of an object in this chapter, we’ll refactor the code to add other physics
processes in chapters 3 and 4, which will allow the simulated water to flow more
realistically. Implementing the other processes will be easier because we’ll be able
to reuse much of the code that we’ll write in this chapter.
 We’ll start off by compiling, linking, and running your first Fortran program.
Then I’ll introduce the physical problem that we want to solve and show you how
to express it in the form of a computer program. We’ll then dive into the essential
elements of Fortran: data types, declaration, arithmetic, and control flow. At the
This chapter covers
 Compiling and running your first Fortran program
 Data types, declaration, arithmetic, and 
control flow
 Building and running your first simulation app
27
Compiling and running your first program
end of the chapter, you’ll have the working knowledge to write basic, yet useful, For-
tran programs.
2.1
Compiling and running your first program
Let’s start by creating, compiling, and running your first Fortran program. I assume
you’ve already installed the GNU Fortran compiler (gfortran) on your system. If you
haven’t yet, follow the directions in appendix A to get yourself set up.
 When you have the compiler installed, test it by compiling and running your first
Fortran program, as shown in the following listing.
program hello    
print *, 'Hello world!'       
end program hello      
This program does only one thing—it prints a short greeting message to the terminal—
as is common for the first example in most programming books. Let’s save it in a file
called hello.f90. Compiling is as simple as passing the source file to the compiler and,
optionally, specifying the name of the output (-o) executable:
gfortran hello.f90 -o hello
If you don’t specify the name of the output file with -o, the name of the executable
defaults to a.out.
 Running the program produces the expected output:
./hello
     
Hello world!    
That’s it—you wrote and compiled your first Fortran program! Let’s take a look at
what happens here under the hood. Building a program typically involves two steps:
1
Compiling—The compiler parses the source code in a high-level language (here,
Fortran) and outputs a corresponding set of machine instructions. In our case,
gfortran will read a Fortran source file with a .f90 suffix and output a corre-
sponding binary object file with a .o suffix. Other suffixes for source files, like
.f, .f03, or .f08, are acceptable by most compilers; however, I recommend
sticking with .f90 for consistency.
2
Linking—Binary object files (.o), which are the result of the compilation step,
aren’t executable on their own. The linker, typically invoked by the compiler
under the hood, puts binary object files together into one or more executable
programs.
Listing 2.1
Your first Fortran program: hello.f90
Begins the program 
and gives it a name
Prints a short greeting 
to the terminal
Ends the program
Runs the program by entering 
the executable name
The output of the 
program in the terminal
28
CHAPTER 2
Getting started: Minimal working app
To build our first program, we issued only one command, gfortran hello.f90 -o
hello, meaning there weren’t two separate steps for compiling and linking. This is
sufficient when the whole program is contained in a single file, and compiling and
linking steps are combined together in one command. That command is equivalent to
the following listing.
gfortran -c hello.f90
   
gfortran hello.o -o hello    
In this snippet, the compiler option -c means compile only, do not link. This procedure
is necessary whenever we need to compile multiple source files before linking them
into a single program. As your app or library grows in size, you’ll find that splitting it
into multiple files will make it easier to organize and further develop.
 I illustrate the build sequence in figure 2.1.
The GNU Fortran compiler can take many other options that control language rules,
warning messages, optimization, and debugging. I encourage you to go ahead and
skim through the manual. You can access it by typing man gfortran on the command
line. If the manual pages aren’t available on your system, you can always access the
most recent documentation for gfortran at https://gcc.gnu.org/onlinedocs/gfortran. 
2.2
Simulating the motion of an object
Near the end of the previous chapter, I introduced the shallow water system of equa-
tions, which we’ll work to solve over the course of this book to produce a realistic sim-
ulation of a tsunami. Here we’ll start implementing the simulator from scratch, both
in terms of the source code and the physics that we’ll simulate with it. The first process
Listing 2.2
Compilation and linking as separate steps
Compiles only, no linking
Links the object 
to an executable
hello.f90
hello.o
hello
Linking step
gfortran hello.o -o hello
gfortran -c hello.f90
Compiling step
Source code
Binary object
Binary executable
Figure 2.1
Compiling and linking steps 
that take the input source code and 
generate binary object and executable 
files. The source file, hello.f90, is passed 
to the compiler, which outputs a binary 
object file, hello.o. The object file is then 
passed to the linker, which outputs a 
binary executable file, hello. The linker is 
implicitly included in the compiler 
command (gfortran).
29
Simulating the motion of an object
that we’ll simulate is the motion of an object due to background flow. In physics, we
call this linear advection. Advection means movement through space, and the linear
property implies that the background flow is independent from the shape and posi-
tion of the object. Don’t worry if you’re not a math or physics whiz and this sounds
daunting! In the following subsections, I’ll illustrate how advection works and show
how you can calculate it without having to understand all the math behind it.
In the next subsection, I’ll state the problem and set some requirements for our app.
Then, I’ll guide you through an illustrative example of advection and show how you
can calculate it yourself without writing any code. Finally, we’ll work together to imple-
ment the first version of our app in the remainder of this chapter.
2.2.1
What should our app do?
At this stage, we’ll simulate only the movement of an object (or fluid) due to back-
ground flow. This will provide the foundation for other physical processes that we’ll
add to the solver in later chapters. Simulating only one process for now will guide the
design of our program structure and its elements: declaration and initialization of
data, iterating the simulation forward in time, and writing the results to the terminal.
I sketched the result that we expect in figure 2.2.
From calculus to code
If you want to delve deeper into the math behind this problem, head over to appendix
B. There, I explain the gradient, which is the key concept behind advection, and how
to express it in computer code using finite differences. This step is important, as it
forms the foundation to express all other terms in the shallow water equations. Oth-
erwise, if you want to skip the math and jump straight to programming, carry on!
Moving an object forward in space
Height
Initial time
Future time
Distance
Background ﬂow
Figure 2.2
Advecting an object in space from left to right. The initial state is on 
the left. The object is advected from left to right by a background flow and after 
some time arrives at its final position on the right.
30
CHAPTER 2
Getting started: Minimal working app
Note that the advected object can be any quantity, such as water height, temperature,
or concentration of a pollutant. For now, we’ll just refer to it as the object for simplicity.
The shape of the object is also arbitrary—it can be any continuous or discontinuous
function. I chose a smooth bulge for convenience. At the initial time, the object is
located near the left edge of the domain. Our goal is to simulate the movement of the
object due to background flow and record the state of the object at some future time.
Internally, our app will need to perform the following steps:
1
Initialize—Define the data structure that will keep the state of the object in com-
puter memory, and initialize its value.
2
Simulate—This step will calculate how the position of the object will change
over time. At this stage, we expect it only to move from left to right, without
change in shape. The simulation is done over many discrete time steps and
makes up for most of the compute time spent by the program.
3
Output—At each time step, we’ll record the state of the object so that we can
visualize it with an external program.
As you can guess, the core of our program will revolve around the simulation step.
How do we go about simulating the movement of the object? Before writing any code,
we need to understand how advection works. 
2.2.2
What is advection?
Wikipedia defines advection as “the transport of a substance or quantity by bulk
motion.” Advection is a fundamental process in physics, engineering, and earth sci-
ences. It governs how a solid object or a fluid moves in space because of background
flow. When a swimmer is swimming against the current, they’re advected by the cur-
rent, and their speed relative to the ground is lower than if there were no current at
all. Advection is also why we find Saharan dust in the atmosphere over the Carib-
bean, Brazil, or northern Europe, or why garbage patches converge in the middle of
ocean basins.
 I mentioned earlier that in this chapter we’ll deal only with linear advection. The
word linear here means that the background flow can be assumed to be constant, and
not changing because of interactions with the advected object itself. As shown in fig-
ure 2.2, the object is moving with constant speed that’s independent from the object
itself. In other words, the shape and position of the object do not influence the back-
ground flow. In the real world, however, this is almost never the case! Nonlinear advec-
tion of velocity is what creates turbulence. Small vortices in a stream, occasional bumps
on commercial flights, and marbled texture that we see in photographs of Jupiter’s
atmosphere are all examples of turbulence caused by nonlinear advection on differ-
ent spatial scales. We’ll save the nonlinear advection for chapter 4; here, we’ll focus
only on the linear part.
 To better understand how advection works, consider a cold front moving across the
southeast United States (figure 2.3). A cold front is a large-scale weather phenomenon
31
Implementing the minimal working app
associated with mid-latitude cyclones. It typically moves from northwest to southeast in
the Northern Hemisphere (southwest to northeast in the Southern Hemisphere) and
brings cool and dry air in its wake. Where I live in South Florida, passages of cold fronts
are eagerly anticipated because they bring refreshingly cool and dry air from the north.
 Now I have a little exercise for you. Consider the following:
 The temperature is 12 °C in Atlanta and 24 °C in Miami.
 The distance between Atlanta and Miami is 960 kilometers.
 The front is moving toward Miami at a constant speed of 20 kilometers per
hour (km/h).
Assume there are no other processes at play, and the change of temperature is uni-
form in space:
1
What is the temperature gradient between Atlanta and Miami? Gradient is the
difference of a quantity (here, temperature) between two locations, divided
by the distance between them. In this case, the temperature gradient has units
of °C/km.
2
How many hours will it take for the temperature in Miami to fall to 12 °C?
3
Finally, what will the temperature in Miami be after 24 hours? How did you
arrive at this result?
Try to solve this problem with pen and paper. After you’ve worked through the exer-
cise, you’ll have solved the linear advection equation, even if you didn’t realize it. The
advection equation predicts the change of any quantity due to the spatial gradient of
that quantity and the background flow. We’ll do the exact same calculation to predict
the motion of the object in our simulator. You can find the solution to this exercise in
the “Answer key” section near the end of this chapter. 
2.3
Implementing the minimal working app
Having set the problem to solve, we’ll soon be able to dive into Fortran coding. But
first we’ll go over the implementation strategy (you should always have one) in the
next subsection. Then, we’ll go over the core elements of the language and apply
them to implement the first version of the tsunami simulator.
Atlanta
12 °C
Miami
Warm
Cold
18 °C
24 °C
Figure 2.3
An illustration of a cold front moving from 
Atlanta toward Miami. Curved lines are contours of 
constant temperature. The dashed arrow shows the 
direction of front propagation.
32
CHAPTER 2
Getting started: Minimal working app
2.3.1
Implementation strategy
Before we do any coding, it will be helpful to sketch out our tentative strategy for
implementing the first version of our app:
1
Define the main program. This will define the program name and scope. The main
program unit provides a skeleton to hold the declaration of data and the exe-
cutable code, such as arithmetic, loops, and so on.
2
Declare and initialize variables and constants. We need to declare all variables and
constants that we intend to use in our program:
– Integer counters i and n, for space and time, respectively, and correspond-
ing loop dimensions grid_size and num_time_steps. The spatial dimension
size, grid_size, will determine the length of the arrays, whereas the time
dimension size, num_time_steps, will determine for how many iterations
we’ll calculate the solution.
– Physical constants for background flow speed, c, time step, dt, and grid spac-
ing, dx.
– Arrays with real values for water height, h, and its finite difference, dh, such
that dh(i) = h(i) - h(i-1) for each i. The array dh is necessary for comput-
ing the solution without keeping multiple time levels in memory.
3
Calculate the equation solution for a set number of time steps. This step consists of
three distinct parts:
– Loop for a set number of time steps (num_time_steps).
– At each step, calculate the new value for water height, h, based on the value
from the previous time step.
– Because our domain is limited in size (grid_size), we need to define the
boundary conditions. What happens to the object when it reaches the far
right edge of the domain (figure 2.4)?
What happens when the object leaves the domain?
Height
Initial time
Future time
Distance
Background ﬂow
?
Figure 2.4
Boundary conditions determine what happens to the object when it 
reaches an edge of the domain. Should it just leave? Reflect back into the domain 
like a ball bouncing off a wall? Or perhaps cycle and reappear on the left side?
33
Implementing the minimal working app
We have a few choices here. The object could be absorbed by the boundary and
completely leave the domain without a trace, or it could be reflected back into
the domain like a ball bouncing off a wall. Another option is a periodic (or
cyclical) boundary condition that connects the right and left edges of the
domain. In this case, the object would pass through on the right and reappear
on the left. This is a common choice in global atmosphere and ocean predic-
tion because of how our planet is represented in the computational domain. If
you go far enough east, you end up in the west! For this reason, we’ll imple-
ment the periodic boundary condition in our app.
4
Print the solution to the terminal at each step. To start, we don’t need fancy or spe-
cially formatted output. Let’s just output our solution in a default text format to
the screen. If we want to store the output in a file for analysis or plotting, we can
easily direct the output into a file.
Sound good? Let’s dive in and tackle these one at a time. 
2.3.2
Defining the main program
The main program is the fundamental program unit in Fortran. It allows you to
assign a name to your program and defines the program scope, as shown in the fol-
lowing listing.
program tsunami     
end program tsunami     
Assigning a name to a program doesn’t do anything in practice, but it can help you
stay organized when you start working with dozens of different programs.
 Compiling and linking a main program source file results in an executable file that
you can invoke from the host operating system (see figure 2.1). You can’t invoke a
main program from other program units.
Listing 2.3
Defining the program unit and scope
What other program units are there?
Here, I give you a sneak peek of what’s coming in chapters 3 and 4. Different program
units can together form an executable program or a nonexecutable library:
 Main program—Top-level unit that can be invoked only from the operating
system
 Function—An executable subprogram that is invoked from expressions and
always returns a single result
 Subroutine—An executable subprogram that can modify multiple arguments
in-place but can’t be used in expressions
Begins the new program 
and gives it a name
Ends the program
34
CHAPTER 2
Getting started: Minimal working app
The program statement is not mandatory. It can be useful to omit it for short test pro-
grams. However, it’s good practice to include it and pair it with a matching end program
statement. Technically, end is the only required statement for any Fortran program.
That statement also makes the shortest possible, though useless, Fortran program.
TIP
Always pair the program statement with a matching end program statement. 
2.3.3
Declaring and initializing variables
Explicit is better than implicit.
                                                 
—Tim Peters
The first part of any program unit is the declaration section. Fortran employs a static,
manifest, strong typing system:
 Static—Every variable has a data type at compile time, and that type remains the
same throughout the life of the program.
 Manifest—All variables must be explicitly declared in the declaration section
before their use. An exception and caveat is implicit typing, described in the sidebar.
 Strong—Variables must be type-compatible when they’re passed between a pro-
gram and functions or subroutines.
(continued)
 Module—A nonexecutable collection of variable, function, and subroutine
definitions
 Submodule—Extends an existing module and is used for defining variable
and procedure definitions that only that module can access; useful for more
complex apps and libraries
For now, we can work with only the main program. We’ll dive deep into functions and
subroutines in chapter 3, and modules in chapter 4.
Implicit typing
Fortran has a historical feature called implicit typing, which allows variable types to be
inferred by the compiler based on the first letter of the variable. Yes, you read that right.
Implicit typing comes from the early days of Fortran (ahem, FORTRAN), before type
declarations were introduced to the language. Any variable that began with I, J, K, L,
M, or N was an integer, and it was a real (floating point) otherwise. FORTRAN 66 intro-
duced data types, and FORTRAN 77 introduced the IMPLICIT statement to override
the default implicit typing rules. It wasn’t until Fortran 90 that the language allowed
completely disabling the implicit typing behavior by using the statement implicit
none before the declaration.
The implicit none statement will instruct the compiler to report an error if you try
to use a variable that hasn’t been declared. Always use implicit none!
35
Implementing the minimal working app
Intrinsic types are defined by the language standard and are immediately available for
use. Fortran has three numeric types:

integer—Whole numbers, such as 42 or -17

real—Floating point numbers, such as 3.141 or 1.82e4

complex—A pair of numbers: one for the real part and one for the imaginary
part of the complex number; for example, (0.12, -1.33)
Numeric types also come in different kinds. A Fortran kind refers to the memory size
that’s reserved for a variable. It determines the permissible range of values and, in the
case of real and complex numbers, the precision. In general, higher integer kinds
allow a wider range of values. Higher real and complex kinds yield a higher allowed
range and a higher precision of values. You’ll learn more about numeric type kinds in
chapter 4.
 Besides the numerical intrinsic types, Fortran also has the logical type to repre-
sent Boolean (true or false) states and character for text data. These five intrinsic
types (integer, real, complex, logical, and character) are the basis for all variables
in Fortran programs. You also can use them to create compound types of any com-
plexity, called derived types, which are analogous to struct in C and class in Python.
We’ll dive deep into derived types in chapter 8.
TIP
Always use implicit none. This statement enforces explicit declaration
of all variables, which both serves as documentation for the programmer and
allows the compiler to find and report type errors for you. 
2.3.4
Numeric data types
Fortran provides three numerical data types out of the box: integer, real, and complex.
INTEGER NUMBERS
The integer is the simplest numeric type in Fortran. Here are some examples of inte-
ger literals:
0
1
42
100
-500
+1234567
You declare one or more integers like this:
integer :: i, n
This statement instructs the compiler to reserve the space in memory for integer vari-
ables i and n. It’s made of the type statement (integer) on the left, double colons
(::) in the middle, and a list of variables separated by commas.
 General rules for integers:
 Integers are always signed—they can be both negative and positive, as well as
zero.
 They have a limited range that’s determined by their type kind. Larger type
kinds result in a wider range.
36
CHAPTER 2
Getting started: Minimal working app
 Exceeding the permissible range of a variable results in an overflow. In that
event, the value of the variable will wrap around its range limits.
 The default integer size in memory isn’t defined by the Fortran standard and is
system dependent. However, on most systems, the default integer size is 4 bytes. 
REAL NUMBERS
Real numbers, also known as floating-point numbers, describe any number that has a
value along a continuous (nondiscrete) axis. Here are some examples of real numbers:
0.0
1.000
42.
3.14159256
-5e2
+1.234567e5
The first four of these are intuitive enough—the decimal point separates the whole
part of the number on the left and the fractional part of the number on the right. The
last two may seem strange, as they’re written using exponential notation. They consist of
an integer or real number on the left side of the character e, and an integer on its
right side that denotes the decimal exponent. -5e2 thus corresponds to –5 × 102, and
+1.234567e5 corresponds to 1.234567 × 105. For positive numbers, the unary plus
symbol is optional. We’ll talk more about formatting real numbers in chapter 6.
We declare real numbers using the keyword real:
real :: x
This declaration statement is analogous to the one for integers, except for the type
and variable names. 
COMPLEX NUMBERS
A complex number is simply a pair of real numbers, one for the real component and
one for the imaginary component. They’re declared and initialized like this:
complex :: c = (1.1, 0.8)
The complex intrinsic type was introduced into Fortran to make arithmetic with com-
plex numbers easier to program. Depending on your application, you’ll either use
them often or not at all. 
Be mindful about the decimal point!
When writing literal constants, there’s a fine line between what the compiler will
understand as an integer or a real. A single period after the number makes the dif-
ference. For example, 42 is an integer, but 42. is a real. This is the same behavior
as in C or Python.
37
Implementing the minimal working app
2.3.5
Declaring the data to use in our app
Now that you have an idea of how to declare a variable of a specific numeric type, let’s
declare some variables, constants, and arrays that we’ll use in the tsunami simulator.
DECLARING VARIABLES
What kinds of variables will we need? As a reminder, based on our implementation
strategy in section 2.3.1, we’ll need the following:
 Spatial array size, grid_size, and number of time steps, num_time_steps
 Physical constants, such as time step, dt, grid size, dx, and background flow
speed, c
 One-dimensional arrays to carry the values of water height, h, and its difference
in space, dh
 An integer index, i, to reference individual array elements, h(i), and another
to loop through time, n
Since we need to first specify grid_size before we declare the array h, let’s first
declare scalar variables and constants, and declare the arrays afterward, as shown in
the following listing.
program tsunami
implicit none        
integer :: i, n
   
integer :: grid_size
   
integer :: num_time_steps   
real :: dt ! time step [s]
   
real :: dx ! grid spacing [m]   
real :: c ! phase speed [m/s]   
grid_size = 100
   
num_time_steps = 100   
dt = 1.   
dx = 1.   
c = 1.    
end program tsunami
The declaration section begins with implicit none and ends immediately before the
first executable line of code (grid_size = 100). All declarations are done in one
place, at the beginning of the program.
 
 
Listing 2.4
Declaring and initializing integer and real variables
Enforces 
explicit typing
Integer 
declarations
Real (floating point) 
declarations
Initializes 
integers
Initializes 
reals
38
CHAPTER 2
Getting started: Minimal working app
For variables that won’t change value for the duration of the program, it’s useful to
declare them as constants. This allows the compiler to better optimize the code and
prevents you from accidentally changing the value of a constant. We’ll declare con-
stants in the next section.
 Our program won’t do much for now, as we only have the data declarations in it.
However, feel free to tweak it, recompile it, and, even better, try to break it! See if the
compiler complains. 
DECLARING CONSTANTS
Some of the variables will be constant, and Fortran allows you to declare them as such
explicitly. Doing so will help you write correct code by triggering a compiler error if
you try to change the value of a constant, and will help the compiler optimize the
code. You can declare a constant (also known as immutable) using the parameter attri-
bute, as shown in the following listing.
integer, parameter :: grid_size = 100
     
integer, parameter :: num_time_steps = 100     
real, parameter :: dt = 1, dx = 1, c = 1    
Using the parameter attribute requires us to initialize the variable on the same line.
TIP
If the value of a variable is known at compile time and won’t change for
the duration of the program, declare it as a parameter. 
DECLARING ARRAYS
Arrays are among Fortran’s most powerful features. They have several useful properties:
 Contiguous—Array elements are contiguous in memory. Indexing them and
performing element-wise arithmetic on arrays is extremely efficient on modern
processors.
Commenting the code
Fortran comments begin with an exclamation mark (!). They can start at the beginning
of the line, or they can follow any valid statement.
Ideally, your code should be clear enough that it doesn’t need any comments. How-
ever, this is often not possible, and most programs need at least some comments.
Use your best judgment. If the intent isn’t obvious from the code itself, describe it in
comments.
Finally, having no comment is always better than having an inaccurate or outdated
comment.
Listing 2.5
Declaring and initializing constants
Declares grid size as a 
constant parameter
Ditto for the number 
of time steps in the 
simulation
Time step in seconds (s), grid spacing in meters (m),
and background flow speed in meters per second (m/s)
39
Implementing the minimal working app
 Multidimensional—The Fortran standard allows up to 15 dimensions for arrays.
In contrast, in C you have to emulate multiple dimensions by defining arrays
of arrays.
 Static or dynamic—Fortran arrays can be static, with dimensions set at compile
time, or dynamic, with dimensions set at runtime.
 Whole-array arithmetic—You can use the usual scalar arithmetic operators and
mathematical functions with arrays as well.
 Column-major indexing—Fortran arrays use column-major indexing, like MAT-
LAB or R, and unlike C or Python, which are row-major. The first (leftmost)
index thus varies fastest. For example, a(1,1), a(2,1), a(3,1), and so on, are
consecutive elements of array a. Keep this in mind when you loop over ele-
ments of a multidimensional array.
Having declared the integer grid size as a parameter, we can use it to set the size
of the array, h, that holds the water height values. You can declare a fixed-length
(static) real array using the dimension attribute, and an integer parameter for the
array size:
real, dimension(grid_size) :: h    
The argument to dimension is the integer length of the array—in our case, the param-
eter grid_size.
As I mentioned earlier, one of Fortran’s strengths is its intrinsic support for multidi-
mensional arrays. You can define an array of up to 15 dimensions by specifying it in
the declaration statement, for example:
real, dimension(10, 5, 2) :: h
Here, h is declared as a three-dimensional array, with a total of 100 elements (10 * 5 * 2).
 
Shorthand syntax for declaring arrays
You can declare arrays in an even shorter form by omitting the dimension attribute
and specifying the array length in parentheses immediately after the array name:
real :: h(grid_size)
Whether you use the dimension attribute or the more concise form is completely up
to you. However, to conserve space in code listings, I’ll use the shorthand syntax
throughout this book.
Declares h as a real array with the 
number of elements equal to grid_size
40
CHAPTER 2
Getting started: Minimal working app
For now, we need two arrays in our app—one for water height, h, and another for its
finite difference, dh:
real :: h(grid_size), dh(grid_size)
Now that we have our data structures declared and ready for action, let’s see what we
can do with them. 
2.3.6
Branching with an if block
One of the key elements of almost every computer program is taking different execu-
tion paths (branches) depending on some criterion. Take, for example, a program
that parses a bank account registration form for a customer. If one of the input fields
isn’t entered correctly, such as a Social Security number having letters or a name hav-
ing numbers, the program should alert the user and ask for correct input rather than
proceeding. You can define this program behavior using an if block. In our tsunami
simulator, for now we’ll use an if block to check the values of the input grid and phys-
ics parameters, as shown in the following listing.
if (grid_size <= 0) stop 'grid_size must be > 0'
if (dt <= 0) stop 'time step dt must be > 0'
if (dx <= 0) stop 'grid spacing dx must be > 0'
if (c <= 0) stop 'background flow speed c must be > 0'
Here, we check the values of the parameters to make sure the program can carry out
a meaningful simulation. Specifically, we need a grid with at least one element,
although this won’t make for a particularly interesting simulation. We also need time
step, grid spacing, and background flow speed to all have positive values. The condi-
tions are stated in parentheses, immediately after if. On the right side, we specify the
statement to be executed if the condition in parentheses evaluates as true. In this case,
we use the stop statement to abort the program execution and print a helpful mes-
sage for the user.
How about dynamic arrays?
You may have noticed that in both array declarations, we used integer literals to set
the size and shape of the array. However, what if our array dimensions are not known
until runtime? Fortran provides excellent support for dynamic arrays, also known as
allocatable arrays. When you declare an allocatable array, you only specify the rank
(number of dimensions) of the array in the declaration, not the size of the dimen-
sions. Once the size is known, the allocate statement is used to allocate the array
with specified dimensions. Allocatable arrays can also be reallocated dynamically any
number of times. You’ll see more on allocatable arrays in chapter 5, where we’ll put
them to good use in our app.
Listing 2.6
Checking for values of input parameters
41
Implementing the minimal working app
 This is only the simplest kind of use case for an if statement. Here’s its general
syntax:
if (condition) ...
You can use a more verbose if block if you need to execute multiple statements on a
condition, as shown in the following listing.
if (condition) then
...         
end if
So far, the statements inside this if block execute only on a condition that evaluates as
true, and nothing happens otherwise. If we need our program to do something in
either case, we can use a more general if/else/end if block, as shown in the follow-
ing listing.
if (condition) then
...       
else
...    
end if
Unlike the single-liner if and the if/end if block, this one allows for two branches of
execution: one if condition is true, and another if it’s false. It’s also possible to test for
multiple specific conditions in a single if block, as shown in the following listing.
if (condition) then
...
else if (other_condition) then    
...
else
...
end if
The conditions are expressions of the logical type. The comparison operators, like
the ones we used to check the values of the input parameters, work just like the com-
parison operators in general arithmetic we learned in elementary school. There are a
few other edge cases and logical operators that I’ll put on the back burner for now,
and that we’ll explore later as we encounter them.
 In summary, we have a few different forms of an if-block:
1
if single-liner—Useful for simple checks and statements that fit on a single line;
for example, zeroing a variable if negative: if (a < 0) a = 0
Listing 2.7
General syntax of an if block with one condition and one branch
Listing 2.8
General syntax of an if block with one condition and two branches
Listing 2.9
The most general syntax of an if block
This branch will execute 
if condition is true.
This branch will execute 
if condition is true.
This branch will 
execute otherwise.
You can have as many 
of these as you want.
42
CHAPTER 2
Getting started: Minimal working app
2
if/end if—A more verbose form of the single-line if; useful when you have a
single condition but multiple statements to execute
3
if/else/end if—Allows executing a statement for either the true or false value
of the condition
4
if/else if/else/end if—Like if/else/end if, but allows checking the val-
ues of multiple specific conditions
That’s all you need to know about branching for now. We’ll apply these more complex
if blocks in the following chapters. 
2.3.7
Using a do loop to iterate
We need to implement looping in our app to do two things:
1
Loop over array elements to set initial values of water height and calculate the
solution at the next time step.
2
Loop for a number of time steps to iterate the numerical solution forward
in time.
The main construct for looping or iterating in Fortran is the do loop:
do n = start, end     
...     
end do
Here, n is the integer index that changes value with each iteration. In the first itera-
tion, n equals start. When the program reaches the end do line, n is incremented by
one. Internally, the program then checks if n is larger than end. If yes, the program
breaks out of the loop and proceeds to the code that follows the loop. Otherwise, the
control is returned to the beginning of the loop and another iteration is done. The
process repeats until the program exits the loop.
 By default, do loops increment the counter by one. However, you can specify a cus-
tom integer increment immediately after the end index:
do n = start, end, increment    
...      
end do
In this case, the loop begins with n equal to start and is incremented by the value of
increment with each iteration.
 There are several rules to remember when coding do loops:
 The loop index (n) must be an integer variable (not a parameter).

start, end, and increment must be integers of either sign. They can be vari-
ables, parameters, or expressions that evaluate to integer values.
Increment n from 
start to end.
Code inside the loop will execute 
end - start + 1 times.
Increment n from start 
to end by increment.
Code inside the do loop will execute 
(end - start) / increment + 1 times.
43
Implementing the minimal working app
 If start equals end, the loop body will execute only once.
 If start is greater than end and increment is positive, the loop body will not
execute.
 If start is less than end and increment is negative, the loop body will not execute.
 A bare do statement without the counter and start and end indices is an
infinite do loop that has to be broken out of by other means, such as the exit
statement.
 Loops can be nested (loops inside loops).
 Loops can be named. This is useful for nested loops where you want to associate
an end do with its matching do, as shown in the following listing.
outer_loop: do j = 1, jm
  
inner_loop: do i = 1, im
    
print *, 'i, j = ', i, j      
end do inner_loop
    
end do outer_loop
   
Although naming loops may at first seem unnecessarily verbose, the names
become useful in larger programs with multiple levels of nesting. Furthermore,
you can use loop names to break out of a specific do loop from any level using
the exit statement.
Finally, the general syntax form of a do loop is shown in figure 2.5.
In the figure, expr1, expr2, and expr3 are start and end indices (inclusive), and the
increment, respectively. name can be any given name. 
Listing 2.10
Using names with nested do loops
Begins a named outer
loop (slower varying)
Begins a named inner 
loop (faster varying)
This could be any code 
that we want to iterate.
Closing the inner loop
Closing the outer loop
[name: ] do [var = expr1, expr2[, expr3]]
end do [name]
Optionally, give
your loop a name.
Code to iterate
goes here.
Counter
End
Start
Increment
Figure 2.5
General syntax of a Fortran do loop. Optional syntax elements 
are in square brackets.
44
CHAPTER 2
Getting started: Minimal working app
2.3.8
Setting the initial water height values
Before iterating the solution forward in time, we need to specify the initial state of the
water height. In other words, we need to set the initial values of the array h. A com-
mon choice in toy models like ours is a Gaussian (bell) shape (figure 2.6).
This is a well-defined, smooth function that we can calculate using the exponential, as
shown in the following listing.
integer, parameter :: icenter = 25     
real, parameter :: decay = 0.02
     
do i = 1, grid_size                     
h(i) = exp(-decay * (i - icenter)**2)     
end do
Here, we have the first practical use of the following:
 A do-loop to iterate over array elements. Since we’ve declared h as an array of
size grid_size, this loop will iterate over all elements of the array.
 Arithmetic operators -, *, and ** (power).
 An intrinsic mathematical function exp (exponential). This and other intrinsic
math functions are readily available to use in Fortran programs and don’t need
to be imported in any special way.
 Arithmetic assignment (=) of the result of the expression on the right side to
the variable on the left side. The value of the left side is updated only after the
whole expression on the right side has been evaluated.
Listing 2.11
Initializing the water height with a Gaussian shape
Initial state,
( , 0)
h x
Height
Spatial grid index
1.00
0.00
100
75
50
25
0.25
0.50
0.75
Figure 2.6
Initial values of water height
Central index and decay 
factor of the shape
Loops over all elements, 
from index 1 to grid_size
Calculates the value and assigns it
to each element of the array
45
Implementing the minimal working app
Parameters icenter and decay control the position and width of the water height per-
turbation, respectively. The integer icenter is the array index at which the perturba-
tion is centered. For example, when i equals icenter, the exponent argument
reduces to zero, and h(i) equals 1. The real parameter decay determines the rate of
exponential decay. Larger values yield a thinner perturbation.
2.3.9
Predicting the movement of the object
We initialized the values of water height and are ready to get to the core of our simula-
tion—iterating forward in time and calculating the solution at each time step. This
consists of two steps:
1
Calculate the spatial difference of water height (dh), including the periodic
boundary condition.
2
Use dh to calculate the new values of water height h.
The following listing provides the core of the solver.
time_loop: do n = 1, num_time_steps     
dh(1) = h(1) - h(grid_size)     
do i = 2, grid_size
dh(i) = h(i) - h(i-1)      
end do
do i = 1, grid_size
h(i) = h(i) - c * dh(i) / dx * dt     
end do
end do time_loop
The outer loop, what we call time_loop, increments the integer n from 1 to num_
time_steps. Although we’re not using n anywhere inside the body of the loop, we use
this loop to repeat the body for num_time_steps times. Inside the time_loop, we per-
form two calculations:
Can our array assignment be done in parallel?
Recall our discussion of embarrassingly parallel problems in the previous chapter.
We said that a problem is embarrassingly parallel if there’s no data dependency
between individual iterations. Take a look at our expression for the initial value of h.
Could we distribute this workload among multiple processors?
Begin the practice of asking that question for every computational problem, formula,
or equation that you encounter. Over time, you’ll find more opportunity to distribute
the computation, or at least mark sections of the code that are safe for the compiler
to vectorize. Fortran offers a special do loop for this purpose, called do concurrent.
It guarantees to the compiler that there’s no dependency between individual itera-
tions and that they can be executed out of order, as we’ll see in the next subsection. 
Listing 2.12
Iterating the solution forward in time
Iterates over num_time_steps 
time steps
Applies the periodic boundary 
condition on the left
Calculates the finite 
difference of h in space
Evaluates h at the 
next time step
46
CHAPTER 2
Getting started: Minimal working app
1
We calculate the difference of h in space and store it in the array dh. We do this
in two separate steps:
a
We calculate the value of dh(1), which corresponds to the element on the left
edge of the domain. Because we’re applying periodic (cyclic) boundary condi-
tions, dh(1) depends on the value of h from the right edge of the domain.
b
We loop over the remaining elements (from 2 to grid_size) and set dh(i)
to the difference of h in space (h(i) - h(i-1)).
2
Once we have the array dh computed, we use it to compute the new value of h
and update it. Here we don’t need to store the value of h for every time step,
and we overwrite the old values with new ones.
Fortran follows the same operator precedence rules as general arithmetic:
 Exponentiation (**) is evaluated first.
 Multiplication (*) and division (/) are evaluated second.
 Addition (+) and subtraction (-) are evaluated last.
 Finally, the precedence can be overridden with parentheses.
Furthermore, Fortran operations of equal precedence are evaluated left to right. For
example, this expression
h(i) = h(i) - c * dh(i) / dx * dt
is equivalent to this one:
h(i) = h(i) - (((c * dh(i)) / dx) * dt)
A few pages back, I asked you if it’s possible to parallelize this loop in a trivial way:
do i = 1, grid_size
h(i) = exp(-decay * (i - icenter)**2)
end do
What you should look for is whether any iteration depends on data calculated in any
other iteration. Here, the right side depends only on the loop counter i and parame-
ters decay and icenter, whereas the variable on the left side (h(i)) is not used on
the right side. Could every iteration be carried out in any order without changing the
final result? If yes, the computation can be easily parallelized.
 The first step is to inform the compiler that this section of the code can be exe-
cuted in any order it finds most optimal. Fortran 2008 introduced the do concurrent
construct for this purpose. This construct uses a slightly modified syntax, as shown in
the following listing.
do concurrent (i = 1:grid_size)
h(i) = exp(-decay * (i - icenter)**2)
end do
Listing 2.13
Using do concurrent for embarrassingly parallel calculations
47
Implementing the minimal working app
Here, we use a (i = 1:grid_size) syntax instead of i = 1, grid_size. We’ll cover this
in more detail in chapter 6, but for now we’ll use this syntax to promote all our paral-
lelizable loops to do concurrent.
Inside of time_loop, can you find any other loops that could be expressed using do
concurrent? If yes, use the modified syntax to rewrite them as do concurrent loops. 
2.3.10 Printing results to the screen
We now have the time loop that iterates the solver for exactly num_time_steps time
steps. The last remaining step in this chapter is to print the results to screen. The sim-
plest approach is to print the results to the terminal, from where we can redirect the
output to a file for later use, such as plotting. For this, you can use the print state-
ment, which you already encountered in chapter 1:
print *, n, h      
print is the simplest output statement in Fortran. The * symbol that’s placed immedi-
ately after print signifies default formatting, which tells the compiler to use any for-
mat for the data it finds convenient. In most cases, the default format will be
reasonable. As noted, here we’re printing the values of n (integer scalar) and h (real
array with 100 elements) to the screen. This statement will thus output exactly 101 val-
ues to the terminal in a single line.
 We’ll explore Fortran input/output in more detail in chapter 6. For now, print *
is all we need. 
2.3.11 Putting it all together
Finally, we’ve gotten to the exciting part: taking the pieces that we’ve learned and putting
them together into a complete and working program. We’ll first look at the solution of
our program, visualized with a Python script, and then go through the complete code.
What do concurrent is and what it isn’t
What does do concurrent do exactly? It’s a promise from programmer to compiler
that the code inside the loop can be safely vectorized or parallelized. In practice, a
good compiler would do this using a system threading library or SIMD machine
instructions if available.
do concurrent by no means guarantees that the loop will run in parallel! In cases
such as short loops with simple computations, the compiler may determine that
serial execution would be more efficient. We’ll study explicit, distributed-memory par-
allelism with coarrays in chapter 7. For now, we use do concurrent as a note for
both ourselves and the compiler that some regions of the code are safe to parallelize.
Prints values n and h to the 
terminal using default formatting
48
CHAPTER 2
Getting started: Minimal working app
THE RESULT
The numerical solution of our simple app is shown in figure 2.7. From top to bottom,
each panel shows the state of water height at increments of 25 time steps. The top
panel corresponds to the initial condition, as in figure 2.6. The position of the water
Time step 0
Height
1.00
0.00
0.25
0.50
0.75
Time step 25
Height
1.00
0.00
0.25
0.50
0.75
Time step 50
Height
1.00
0.00
0.25
0.50
0.75
Time step 75
Height
Spatial grid index
1.00
0.00
100
75
50
25
0.25
0.50
0.75
Figure 2.7
Predicting the linear advection of an object, with periodic boundary 
conditions. The water height perturbation is advected from left to right with a 
constant speed of 1 m/s. When the water reaches the boundary on the right, it 
reenters the domain from the left.
49
Implementing the minimal working app
height peak in each panel is consistent with the configuration of the physical parame-
ters of the simulation: background flow speed (c = 1), grid spacing (dx = 1), and time
step (dt = 1). In the bottom panel, we can see the water height peak moving out on
the right and reentering on the left. This confirms that our periodic boundary condi-
tion works as intended.
 Although Fortran is great for high-performance numerical work, it’s less elegant
for graphics and visualization of data. For brevity and simplicity, I use Python scripts
for visualization of the tsunami results. You can find the visualization code in the tsu-
nami repository on GitHub (https://github.com/modern-fortran/tsunami), along-
side the Fortran source code in each chapter directory.
COMPLETE CODE
The complete code for the first version of our tsunami simulator is given in the follow-
ing listing.
program tsunami     
implicit none       
integer :: i, n                                         
integer, parameter :: grid_size = 100
                
integer, parameter :: num_time_steps = 100              
real, parameter :: dt = 1 ! time step [s]
  
real, parameter :: dx = 1 ! grid spacing [m]
    
real, parameter :: c = 1 ! background flow speed [m/s]  
real :: h(grid_size), dh(grid_size)                     
integer, parameter :: icenter = 25                      
real, parameter :: decay = 0.02
                      
if (grid_size <= 0) stop 'grid_size must be > 0'
    
if (dt <= 0) stop 'time step dt must be > 0'
    
if (dx <= 0) stop 'grid spacing dx must be > 0'
    
if (c <= 0) stop 'background flow speed c must be > 0'    
do concurrent(i = 1:grid_size)
h(i) = exp(-decay * (i - icenter)**2)   
end do
print *, 0, h     
time_loop: do n = 1, num_time_steps    
dh(1) = h(1) - h(grid_size)      
Listing 2.14
Complete code of the minimal working tsunami simulator
Beginning of 
the program
Enforces explicit 
declaration of variables
Declaration 
of data
Checks input 
parameter values 
and aborts if invalid
Loops over array elements 
and initializes values
Writes the initial water 
height values to the terminal
Begins
the time
loop
Applies the periodic 
boundary condition at the 
left edge of the domain
50
CHAPTER 2
Getting started: Minimal working app
do concurrent (i = 2:grid_size)
dh(i) = h(i) - h(i-1)       
end do
do concurrent (i = 1:grid_size)
h(i) = h(i) - c * dh(i) / dx * dt     
end do
print *, n, h    
end do time_loop
end program tsunami
With only 30 lines of code, this is a useful little solver! If you compile and run this pro-
gram, you’ll get a long series of numbers as text output on the screen, as shown in the
following listing.
0
9.92950936E-06
2.54193492E-05
6.25215471E-05
...   
1
0.00000000
9.92950845E-06
2.54193510E-05
...     
2
0.00000000
0.00000000
9.92950845E-06
...   
...
Although it may look like nonsense, these are our predicted water height values (in
meters). However, the output is long and will flood your terminal window. You’ll be
able to explore the output more easily if you redirect it to a file, as the following listing
demonstrates.
cd src/ch02     
gfortran tsunami.f90 -o tsunami        
./tsunami > tsunami_output.txt          
python3 plot_height_multipanel.py tsunami_output.txt      
Alternatively, the source code repository on GitHub also comes with a Makefile to
streamline the build process, and you can type make ch02 from the top-level directory.
This also assumes that you’ve set up the Python virtual environment following the
instructions in the README.md file in the repository.
 
 
 
Listing 2.15
Text output of the current version of the tsunami simulator
Listing 2.16
Building, running, and visualizing the output from the tsunami simulator
Calculates the finite difference 
of water height in space
Integrates the solution 
forward; this is the 
core of our solver
Prints current values 
to the terminal
Initial water height output
Output after 
first time 
step
Output after second time step
Enters the source 
code directory
Compiles
the
program
Runs the program 
and redirects the 
output into a 
text file
Visualizes the 
output and 
writes it to 
an image file
51
Going forward with the tsunami simulator
 Go ahead and play with it. Some ideas that come to mind:
 Tweak the initial conditions, perhaps by changing the shape and position of the
initial perturbation. For example, you can change the values of the decay and
icenter parameters, or use a different function when initializing the h array.
Try a sine wave (intrinsic function sin).
 Change the grid size and number of time steps parameters.
Remember that Fortran is a compiled language. Every time you change the code,
you’ll need to recompile it before running it. 
2.4
Going forward with the tsunami simulator
Looking back at this chapter and what we’ve made so far, it’s helpful to summarize
what we don’t have yet and how we’ll get there in the second part of the book:
 In chapter 3, you’ll learn about functions and subroutines, and refactor some of
the calculations in our simulator as reusable procedures.
 In chapter 4, you’ll use Fortran modules to reorganize our app, and you’ll
implement more realistic physics.
 In chapter 5, you’ll learn all about arrays and whole-array arithmetic.
 In chapter 6, we’ll dive deeper into input and output, and you’ll learn how to
output your data in a portable way, format it, and write it to files on disk.
Beyond that, in part 3 of this book we’ll explore parallel computing with coarrays, as
well as advanced data structures and procedures.
A note on abstractions
As we work through this book, we’ll come across new layers of abstraction in each
chapter. Here, an abstraction is a programming mechanism that aims to black-box
the internal implementation away from the programmer. For example, in the next
chapter, functions and subroutines are an abstraction over explicit, imperative code.
In chapter 8, you’ll learn about derived types, which can contain any number of vari-
ables and procedures attached to them, and this is yet another layer of abstraction.
Each layer of abstraction introduces a benefit and a cost. The benefit usually boils
down to having to write less boilerplate code, especially when programming repet-
itive tasks. The cost is that abstractions hide not only the implementation details,
but also meaning and side effects if they’re not used conservatively and with care.
I’ll warn you each time we come across a new abstraction in this book. Consider
each abstraction carefully, and use them only if the benefits outweigh the per-
ceived costs. 
52
CHAPTER 2
Getting started: Minimal working app
2.5
Answer key
This section contains the solution to the exercise in this chapter. Skip ahead if you
haven’t worked through the exercise yet.
2.5.1
Exercise: Cold front propagation
1
What is the temperature gradient between Atlanta and Miami? Here, the gradi-
ent will be the difference in temperature between the two locations, divided by
the distance between them. The answer is thus 24 °C minus 12 °C, divided by
960 km: 0.0125 °C/km.
2
How many hours will it take for the temperature in Miami to reach 12 °C? Let’s
first get the rate of cooling in Miami. The front is moving with the speed of 20
km/h, and we know that the gradient is 0.0125 °C/km. The cooling rate is then
20 km/h times 0.0125 °C/km: 0.25 °C/hour. The Miami temperature starts at
24 °C, so it will reach 12 °C in 24 °C minus 12 °C, divided by the cooling rate
0.25 °C/hour. The result is 48 hours.
3
What will be the temperature in Miami after 24 hours? We know that the Miami
temperature starts at 24 °C and that its cooling rate is 0.25 °C/hour. The answer
is then 24 °C - 0.25 °C/hour times 24 hours. The result is 18 °C. 
2.6
New Fortran elements, at a glance

program/end program statements to define a main program 
 Intrinsic numeric types integer, real, and complex

dimension attribute to declare an array
 Arithmetic operators +, -, *, /, and **, and assignment =

if statement and if blocks for branching 

stop statement to abort the program and print a message to the terminal

do/end do construct to iterate over any executable section of the code

do concurrent to mark embarrassingly parallel sections of the code

print * statement as the simplest way to print text and variable values to the
terminal 
2.7
Further reading
 GNU Fortran compiler documentation: https://gcc.gnu.org/onlinedocs/gfortran
 Wikipedia article on advection: https://en.wikipedia.org/wiki/Advection
 Wikipedia article on finite differences: https://en.wikipedia.org/wiki/Finite_
difference
53
Summary
Summary
 Building an executable Fortran program consists of the compilation and link-
ing steps.
 There are five program units in Fortran: main program, function, subroutine,
module, and submodule.
 A program begins with a program statement and ends with an end program
statement.
 In every program, we first declare the data, and executable code comes after.
 Use implicit none at the top of your declarative code to enforce explicit decla-
ration of all variables.
 There are five built-in data types in Fortran: integer, real, complex, character,
and logical.

if blocks are used to test for conditions and take different execution branches
depending on their values.
 Use the stop statement to abort the program immediately and print a helpful
message to the terminal.

do loops are used to iterate over sections of the code a specified number of times.
 Start, end, and increment values of a do loop counter can have any integer value.
 Fortran’s arithmetic rules are the same as those we learn in school: exponentia-
tion is evaluated first, then multiplication and division are evaluated, and finally
addition and subtraction go last; this order can be overruled with parentheses.

print * is an easy way to print the values of any variable or literal constant to
the terminal.


## Images

