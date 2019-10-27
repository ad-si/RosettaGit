+++
title = "N-body problem"
description = ""
date = 2019-10-04T16:07:48Z
aliases = []
[extra]
id = 17519
[taxonomies]
categories = []
tags = []
+++

{{clarify task}}
{{draft task}}
The [[wp:N-body problem|<math>N</math>-body problem]] is an inherent feature of a physical description of mechanical system of <math>N</math> interacting objects.
The system is described by Newton's Law
: <math>\vec F = m_i \frac{d^2 \vec r_{i}}{dt^2}, i=1..N</math>
with continious combined force from other bodies
: <math>F_i = \sum^{}_{j\neq i} F_{ij}, i=1..N</math>

Exact formulation of first mechanical problem is, given initial coordinates and velocities <math>t=0</math>, to find coordinates and velocities at <math>t=t_0</math> with accuracy more then given relative value <math>\epsilon = \frac{\vec r- \vec r'}{|\vec r|}</math>

As well known from physical background, only the choice of <math>N=2</math> can be analytically solved.

;Task
Write a simulation of three masses interacting under gravitation and show their evolution over a number of time-steps (at least 20).


## C


### A little history

A long, long time ago ( well, more than 10 years, which is a very long time when it comes to computing ), I implemented a [https://en.wikipedia.org/wiki/Borland_Graphics_Interface BGI] program in C for the [http://www.fact-index.com/g/gr/gravity_set.html Mitchell-Green Gravity Set]. A chaotic dynamical system, the Gravity set, MGGS in short, is a plot of the paths of massive bodies as they move under the influence of gravity. Those were the days when I was crazy about Graphics and the MGGS orbits brought to life all of the high school Physics I had learnt. Orbits, oscillations, parabolic and hyperbolic trajectories suddenly came to life on my computer screen. I didn't stop there and even implemented a 3D version in OpenGL. This was on a Pentium-4 system with 512 MB RAM and I remember the machine groaning when the OpenGL simulation ran for even 10 bodies.

----
And then I got a job, and in case you are wondering, yes, I can't find either program anywhere. Sad, because I am sure I implemented far more Physics in them than in these ones. The only bit extra I have built into the two implementations below is collision detection. To keep things simple, all bodies are assumed to be point particles and collisions to be perfectly elastic.


### And Now

One time step is assumed to be unit time, t = 1, hence there is no time variable in the implementations. The TCL example uses a time step of 10^-12, which frankly is too small for the C Standard Library and needs specialized techniques or libraries to be handled. Input data is read from a file and hence you can plot as many bodies as you want, keep in mind that this is computationally intensive, so intensive that on an i5, 8 GB RAM system, I didn't have to use any delays in the Graphics implementation for animation. Ideally such tasks are well suited for parallel processing. That's why Supercomputers are required to study weather, galaxies, black holes and molecules. All systems, big or small, can be reduced to N-body problems or definitely play a big role in their study.


### File Format

For both implementations, the file format is as follows :

```txt

<Gravitational Constant> <Number of bodies(N)> <Time step>
<Mass of M1>
<Position of M1 in x,y,z co-ordinates>
<Initial velocity of M1 in x,,y,z components>
...
<And so on for N bodies>

```
 

### The Implementations


### =Textual or Console=


```C

#include<stdlib.h>
#include<stdio.h>
#include<math.h>

typedef struct{
	double x,y,z;
}vector;

int bodies,timeSteps;
double *masses,GravConstant;
vector *positions,*velocities,*accelerations;

vector addVectors(vector a,vector b){
	vector c = {a.x+b.x,a.y+b.y,a.z+b.z};
	
	return c;
}

vector scaleVector(double b,vector a){
	vector c = {b*a.x,b*a.y,b*a.z};
	
	return c;
}

vector subtractVectors(vector a,vector b){
	vector c = {a.x-b.x,a.y-b.y,a.z-b.z};
	
	return c;
}

double mod(vector a){
	return sqrt(a.x*a.x + a.y*a.y + a.z*a.z);
}

void initiateSystem(char* fileName){
	int i;
	FILE* fp = fopen(fileName,"r");
	
	fscanf(fp,"%lf%d%d",&GravConstant,&bodies,&timeSteps);

	masses = (double*)malloc(bodies*sizeof(double));
	positions = (vector*)malloc(bodies*sizeof(vector));
	velocities = (vector*)malloc(bodies*sizeof(vector));
	accelerations = (vector*)malloc(bodies*sizeof(vector));
	
	for(i=0;i<bodies;i++){
		fscanf(fp,"%lf",&masses[i]);
		fscanf(fp,"%lf%lf%lf",&positions[i].x,&positions[i].y,&positions[i].z);
		fscanf(fp,"%lf%lf%lf",&velocities[i].x,&velocities[i].y,&velocities[i].z);
	}
	
	fclose(fp);
}

void resolveCollisions(){
	int i,j;
	
	for(i=0;i<bodies-1;i++)
		for(j=i+1;j<bodies;j++){
			if(positions[i].x==positions[j].x && positions[i].y==positions[j].y && positions[i].z==positions[j].z){
				vector temp = velocities[i];
				velocities[i] = velocities[j];
				velocities[j] = temp;
			}
		}
}

void computeAccelerations(){
	int i,j;
	
	for(i=0;i<bodies;i++){
		accelerations[i].x = 0;
		accelerations[i].y = 0;
		accelerations[i].z = 0;
		for(j=0;j<bodies;j++){
			if(i!=j){
				accelerations[i] = addVectors(accelerations[i],scaleVector(GravConstant*masses[j]/pow(mod(subtractVectors(positions[i],positions[j])),3),subtractVectors(positions[j],positions[i])));
			}
		}
	}
}

void computeVelocities(){
	int i;
	
	for(i=0;i<bodies;i++)
		velocities[i] = addVectors(velocities[i],accelerations[i]);
}

void computePositions(){
	int i;
	
	for(i=0;i<bodies;i++)
		positions[i] = addVectors(positions[i],addVectors(velocities[i],scaleVector(0.5,accelerations[i])));
}

void simulate(){
	computeAccelerations();
	computePositions();
	computeVelocities();
	resolveCollisions();
}

int main(int argC,char* argV[])
{
	int i,j;
	
	if(argC!=2)
		printf("Usage : %s <file name containing system configuration data>",argV[0]);
	else{
		initiateSystem(argV[1]);
		printf("Body   :     x              y               z           |           vx              vy              vz   ");
		for(i=0;i<timeSteps;i++){
			printf("\nCycle %d\n",i+1);
			simulate();
			for(j=0;j<bodies;j++)
				printf("Body %d : %lf\t%f\t%lf\t|\t%lf\t%lf\t%lf\n",j+1,positions[j].x,positions[j].y,positions[j].z,velocities[j].x,velocities[j].y,velocities[j].z);
		}
	}
	return 0;
}

```

Input file: same data as the TCL example.

```txt

0.01 3 20
1
0 0 0
0.01 0 0
0.1
1 1 0
0 0 0.02
0.001
0 1 1
0.01 -0.01 -0.01

```

Invocation and output (x,y,z denote position components in 3-space, vx,vy,vz denote velocity components) :

```txt

C:\rosettaCode>mggs.exe mggsData.txt
Body   :     x              y               z           |           vx              vy              vz   
Cycle 1
Body 1 : 0.010177	0.000179	0.000002	|	0.010354	0.000357	0.000004
Body 2 : 0.998230	0.998232	0.020002	|	-0.003539	-0.003536	0.020004
Body 3 : 0.010177	0.988232	0.988055	|	0.010354	-0.013536	-0.013889

Cycle 2
Body 1 : 0.020709	0.000718	0.000011	|	0.010710	0.000721	0.000014
Body 2 : 0.992907	0.992896	0.039971	|	-0.007109	-0.007138	0.019935
Body 3 : 0.020717	0.972888	0.972173	|	0.010727	-0.017153	-0.017876

Cycle 3
Body 1 : 0.031600	0.001625	0.000034	|	0.011072	0.001094	0.000033
Body 2 : 0.983985	0.983910	0.059834	|	-0.010735	-0.010835	0.019790
Body 3 : 0.031643	0.953868	0.952235	|	0.011125	-0.020886	-0.021999

Cycle 4
Body 1 : 0.042858	0.002913	0.000081	|	0.011443	0.001481	0.000060
Body 2 : 0.971393	0.971163	0.079509	|	-0.014448	-0.014659	0.019561
Body 3 : 0.042981	0.931039	0.928087	|	0.011552	-0.024772	-0.026299

Cycle 5
Body 1 : 0.054492	0.004595	0.000160	|	0.011826	0.001884	0.000097
Body 2 : 0.955030	0.954509	0.098909	|	-0.018278	-0.018649	0.019238
Body 3 : 0.054766	0.904225	0.899522	|	0.012018	-0.028857	-0.030829

Cycle 6
Body 1 : 0.066517	0.006691	0.000281	|	0.012224	0.002308	0.000145
Body 2 : 0.934759	0.933760	0.117931	|	-0.022265	-0.022849	0.018806
Body 3 : 0.067040	0.873197	0.866280	|	0.012530	-0.033199	-0.035655

Cycle 7
Body 1 : 0.078950	0.009225	0.000456	|	0.012642	0.002759	0.000206
Body 2 : 0.910400	0.908677	0.136456	|	-0.026454	-0.027316	0.018244
Body 3 : 0.079856	0.837662	0.828023	|	0.013101	-0.037871	-0.040861

Cycle 8
Body 1 : 0.091815	0.012227	0.000702	|	0.013086	0.003245	0.000284
Body 2 : 0.881722	0.878958	0.154340	|	-0.030902	-0.032122	0.017523
Body 3 : 0.093281	0.797239	0.784313	|	0.013749	-0.042975	-0.046559

Cycle 9
Body 1 : 0.105140	0.015737	0.001035	|	0.013564	0.003775	0.000383
Body 2 : 0.848429	0.844216	0.171401	|	-0.035684	-0.037362	0.016600
Body 3 : 0.107405	0.751427	0.734579	|	0.014498	-0.048649	-0.052908

Cycle 10
Body 1 : 0.118964	0.019805	0.001481	|	0.014085	0.004362	0.000509
Body 2 : 0.810137	0.803953	0.187408	|	-0.040900	-0.043166	0.015414
Body 3 : 0.122346	0.699554	0.678056	|	0.015384	-0.055097	-0.060138

Cycle 11
Body 1 : 0.133337	0.024498	0.002071	|	0.014662	0.005025	0.000672
Body 2 : 0.766343	0.757509	0.202050	|	-0.046687	-0.049720	0.013868
Body 3 : 0.138268	0.640690	0.613685	|	0.016460	-0.062633	-0.068603

Cycle 12
Body 1 : 0.148327	0.029907	0.002851	|	0.015317	0.005792	0.000888
Body 2 : 0.716377	0.703998	0.214889	|	-0.053246	-0.057302	0.011810
Body 3 : 0.155406	0.573482	0.539941	|	0.017816	-0.071782	-0.078886

Cycle 13
Body 1 : 0.164025	0.036157	0.003887	|	0.016079	0.006709	0.001184
Body 2 : 0.659310	0.642172	0.225282	|	-0.060887	-0.066351	0.008976
Body 3 : 0.174112	0.495836	0.454475	|	0.019596	-0.083511	-0.092045

Cycle 14
Body 1 : 0.180564	0.043437	0.005286	|	0.017000	0.007852	0.001613
Body 2 : 0.593807	0.570186	0.232208	|	-0.070119	-0.077621	0.004875
Body 3 : 0.194929	0.404136	0.353320	|	0.022038	-0.099890	-0.110265

Cycle 15
Body 1 : 0.198150	0.052049	0.007234	|	0.018171	0.009372	0.002283
Body 2 : 0.517817	0.485100	0.233878	|	-0.081861	-0.092550	-0.001535
Body 3 : 0.218605	0.290860	0.228583	|	0.025314	-0.126661	-0.139210

Cycle 16
Body 1 : 0.217126	0.062542	0.010117	|	0.019781	0.011614	0.003484
Body 2 : 0.427899	0.381659	0.226654	|	-0.097974	-0.114332	-0.012913
Body 3 : 0.244268	0.131956	0.057562	|	0.026013	-0.191148	-0.202831

Cycle 17
Body 1 : 0.238346	0.076539	0.015221	|	0.022658	0.016380	0.006723
Body 2 : 0.317489	0.248502	0.200967	|	-0.122846	-0.151982	-0.038461
Body 3 : 0.075592	-0.559591	-0.487315	|	-0.363366	-1.191945	-0.886924

Cycle 18
Body 1 : 0.263123	0.097523	0.026918	|	0.026898	0.025587	0.016672
Body 2 : 0.173428	0.050424	0.112716	|	-0.165275	-0.244174	-0.138041
Body 3 : -0.286241	-1.745597	-1.369528	|	-0.360299	-1.180066	-0.877501

Cycle 19
Body 1 : 0.270854	0.113045	0.061923	|	-0.011436	0.005457	0.053339
Body 2 : 0.199821	-0.093105	-0.208666	|	0.218061	-0.042882	-0.504723
Body 3 : -0.646318	-2.924909	-2.246453	|	-0.359856	-1.178559	-0.876350

Cycle 20
Body 1 : 0.258572	0.116046	0.112038	|	-0.013129	0.000544	0.046890
Body 2 : 0.426346	-0.111425	-0.681150	|	0.234987	0.006241	-0.440245
Body 3 : -1.006089	-4.103186	-3.122591	|	-0.359686	-1.177995	-0.875924


```



### =Graphics=

One of the golden rules of Physics : Draw a diagram. It doesn't have to be artistic, but a properly drawn diagram whose proportions are realistic simplifies the solution of almost every Physics problem. That was the case with this task as well, when I first ran the console version, I noticed that Body 1's position and velocity is varying wildly. I put it down to the chaotic nature of the system and got busy writing the Graphics part and it's when I found that nothing was being drawn on the screen, I took a good look at the implementation and discovered many glaring mistakes. Gravitation is an attractive force, it never repels ( we will talk about [https://en.wikipedia.org/wiki/Dark_energy Dark Energy] later). When I saw the particles moving away from each other, I knew the acceleration vector had the wrong sign.

----
So yes, always draw a diagram, always do a Graphics implementation of any simulation. Input file for this differs from the console one since the BGI screen is the positive or 1st quadrant of the XY-plane. Hence z components are 0, you can use non-zero values but you won't see any change ( or really weird ones ) on the screen.

```txt

0.01 3 20
1.0
100.0 100.0 0.0
0.0 0.0 0.0
2.0
600.0 100.0 0.0
0.0 0.0 0.0
3
500.0 500.0 0.0
0.0 0.0 0.0

```

The time step although present in the file, and read, is not used here, the loop goes on indefinitely. Requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library.

```C

#include<graphics.h>
#include<stdlib.h>
#include<stdio.h>
#include<math.h>

typedef struct{
	double x,y,z;
}vector;

int bodies,timeSteps;
double *masses,GravConstant;
vector *positions,*velocities,*accelerations;

vector addVectors(vector a,vector b){
	vector c = {a.x+b.x,a.y+b.y,a.z+b.z};
	
	return c;
}

vector scaleVector(double b,vector a){
	vector c = {b*a.x,b*a.y,b*a.z};
	
	return c;
}

vector subtractVectors(vector a,vector b){
	vector c = {a.x-b.x,a.y-b.y,a.z-b.z};
	
	return c;
}

double mod(vector a){
	return sqrt(a.x*a.x + a.y*a.y + a.z*a.z);
}

void initiateSystem(char* fileName){
	int i;
	FILE* fp = fopen(fileName,"r");
	
	fscanf(fp,"%lf%d%d",&GravConstant,&bodies,&timeSteps);

	masses = (double*)malloc(bodies*sizeof(double));
	positions = (vector*)malloc(bodies*sizeof(vector));
	velocities = (vector*)malloc(bodies*sizeof(vector));
	accelerations = (vector*)malloc(bodies*sizeof(vector));
	
	for(i=0;i<bodies;i++){
		fscanf(fp,"%lf",&masses[i]);
		fscanf(fp,"%lf%lf%lf",&positions[i].x,&positions[i].y,&positions[i].z);
		fscanf(fp,"%lf%lf%lf",&velocities[i].x,&velocities[i].y,&velocities[i].z);
	}
	
	fclose(fp);
}

void resolveCollisions(){
	int i,j;
	
	for(i=0;i<bodies-1;i++)
		for(j=i+1;j<bodies;j++){
			if(positions[i].x==positions[j].x && positions[i].y==positions[j].y && positions[i].z==positions[j].z){
				vector temp = velocities[i];
				velocities[i] = velocities[j];
				velocities[j] = temp;
			}
		}
}

void computeAccelerations(){
	int i,j;
	
	for(i=0;i<bodies;i++){
		accelerations[i].x = 0;
		accelerations[i].y = 0;
		accelerations[i].z = 0;
		for(j=0;j<bodies;j++){
			if(i!=j){
				accelerations[i] = addVectors(accelerations[i],scaleVector(GravConstant*masses[j]/pow(mod(subtractVectors(positions[i],positions[j])),3),subtractVectors(positions[j],positions[i])));
			}
		}
	}
}

void computeVelocities(){
	int i;
	
	for(i=0;i<bodies;i++)
		velocities[i] = addVectors(velocities[i],accelerations[i]);
}

void computePositions(){
	int i;
	
	for(i=0;i<bodies;i++)
		positions[i] = addVectors(positions[i],addVectors(velocities[i],scaleVector(0.5,accelerations[i])));
}

void simulate(){
	computeAccelerations();
	computePositions();
	computeVelocities();
	resolveCollisions();
}

void plotOrbits(){
	int i;
	
	for(i=0;i<bodies;i++)
		putpixel(positions[i].x,positions[i].y,i%15 + 1);
}

int main(int argC,char* argV[])
{
	int i,j;
	
	if(argC!=4)
		printf("Usage : %s <file name containing system configuration data and width and height of window>",argV[0]);
	else{
		initiateSystem(argV[1]);
		initwindow(atoi(argV[2]),atoi(argV[3]),"N Body Simulation");
		while(1){
			simulate();
			plotOrbits();
		}
	}
	return 0;
}

```



### Further work

And as in every experiment report/journal paper/whatever, time for some philosophizing, a more rigorous implementation will require the bodies to be realistic, this means they will not be point particles and hence the dynamics will be much more complex. Tidal effects, collisions, orbits will require much more computation. When I say realistic, I don't mean rigid bodies because even rigid bodies are an idealization of reality. And all of this, even with classical mechanics, employing the Newtonian version of Gravitation will be a worthy task for any competent Physicist, let alone a Programmer. Introduce the General Theory of Relativity and everything changes. I can go on and on, but this is a coding, not a rambling site. I will end by saying that the Inverse Square Law, a name often used for Newtonian Gravitation, is just one class of forces studied in Dynamics. The force relation can take any form, the N-body problem can therefore be solved in (countably/uncountably ?) infinite ways.


## C++

{{trans|Java}}

```cpp>#include <algorithm

#include <iomanip>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

class Vector {
private:
    double px, py, pz;

public:
    Vector() : px(0.0), py(0.0), pz(0.0) {
        // empty
    }

    Vector(double x, double y, double z) : px(x), py(y), pz(z) {
        // empty
    }

    double mod() const {
        return sqrt(px*px + py * py + pz * pz);
    }

    Vector operator+(const Vector& rhs) const {
        return Vector(px + rhs.px, py + rhs.py, pz + rhs.pz);
    }

    Vector operator-(const Vector& rhs) const {
        return Vector(px - rhs.px, py - rhs.py, pz - rhs.pz);
    }

    Vector operator*(double s) const {
        return Vector(px*s, py*s, pz*s);
    }

    bool operator==(const Vector& rhs) const {
        return px == rhs.px
            && py == rhs.py
            && pz == rhs.pz;
    }

    friend std::istream& operator>>(std::istream&, Vector&);
    friend std::ostream& operator<<(std::ostream&, Vector&);
};

std::istream& operator>>(std::istream& in, Vector& v) {
    return in >> v.px >> v.py >> v.pz;
}

std::ostream& operator<<(std::ostream& out, Vector& v) {
    auto precision = out.precision();
    auto width = out.width();
    out << std::fixed << std::setw(width) << std::setprecision(precision) << v.px << "  ";
    out << std::fixed << std::setw(width) << std::setprecision(precision) << v.py << "  ";
    out << std::fixed << std::setw(width) << std::setprecision(precision) << v.pz;
    return out;
}

const Vector ORIGIN{ 0.0, 0.0, 0.0 };

class NBody {
private:
    double gc;
    int bodies;
    int timeSteps;
    std::vector<double> masses;
    std::vector<Vector> positions;
    std::vector<Vector> velocities;
    std::vector<Vector> accelerations;

    void resolveCollisions() {
        for (int i = 0; i < bodies; ++i) {
            for (int j = i + 1; j < bodies; ++j) {
                if (positions[i] == positions[j]) {
                    std::swap(velocities[i], velocities[j]);
                }
            }
        }
    }

    void computeAccelerations() {
        for (int i = 0; i < bodies; ++i) {
            accelerations[i] = ORIGIN;
            for (int j = 0; j < bodies; ++j) {
                if (i != j) {
                    double temp = gc * masses[j] / pow((positions[i] - positions[j]).mod(), 3);
                    accelerations[i] = accelerations[i] + (positions[j] - positions[i]) * temp;
                }
            }
        }
    }

    void computeVelocities() {
        for (int i = 0; i < bodies; ++i) {
            velocities[i] = velocities[i] + accelerations[i];
        }
    }

    void computePositions() {
        for (int i = 0; i < bodies; ++i) {
            positions[i] = positions[i] + velocities[i] + accelerations[i] * 0.5;
        }
    }

public:
    NBody(std::string& fileName) {
        using namespace std;

        ifstream ifs(fileName);
        if (!ifs.is_open()) {
            throw runtime_error("Could not open file.");
        }

        ifs >> gc >> bodies >> timeSteps;

        masses.resize(bodies);
        positions.resize(bodies);
        fill(positions.begin(), positions.end(), ORIGIN);
        velocities.resize(bodies);
        fill(velocities.begin(), velocities.end(), ORIGIN);
        accelerations.resize(bodies);
        fill(accelerations.begin(), accelerations.end(), ORIGIN);

        for (int i = 0; i < bodies; ++i) {
            ifs >> masses[i] >> positions[i] >> velocities[i];
        }

        cout << "Contents of " << fileName << '\n';
        cout << gc << ' ' << bodies << ' ' << timeSteps << '\n';
        for (int i = 0; i < bodies; ++i) {
            cout << masses[i] << '\n';
            cout << positions[i] << '\n';
            cout << velocities[i] << '\n';
        }
        cout << "\nBody   :      x          y          z    |     vx         vy         vz\n";
    }

    int getTimeSteps() {
        return timeSteps;
    }

    void simulate() {
        computeAccelerations();
        computePositions();
        computeVelocities();
        resolveCollisions();
    }

    friend std::ostream& operator<<(std::ostream&, NBody&);
};

std::ostream& operator<<(std::ostream& out, NBody& nb) {
    for (int i = 0; i < nb.bodies; ++i) {
        out << "Body " << i + 1 << " : ";
        out << std::setprecision(6) << std::setw(9) << nb.positions[i];
        out << " | ";
        out << std::setprecision(6) << std::setw(9) << nb.velocities[i];
        out << '\n';
    }
    return out;
}

int main() {
    std::string fileName = "nbody.txt";
    NBody nb(fileName);

    for (int i = 0; i < nb.getTimeSteps(); ++i) {
        std::cout << "\nCycle " << i + 1 << '\n';
        nb.simulate();
        std::cout << nb;
    }

    return 0;
}
```

{{out}}

```txt
Contents of nbody.txt
0.01 3 20
1
0.000000  0.000000  0.000000
0.010000  0.000000  0.000000
0.100000
1.000000  1.000000  0.000000
0.000000  0.000000  0.020000
0.001000
0.000000  1.000000  1.000000
0.010000  -0.010000  -0.010000

Body   :      x          y          z    |     vx         vy         vz

Cycle 1
Body 1 :  0.010177   0.000179   0.000002 |  0.010354   0.000357   0.000004
Body 2 :  0.998230   0.998232   0.020002 | -0.003539  -0.003536   0.020004
Body 3 :  0.010177   0.988232   0.988055 |  0.010354  -0.013536  -0.013889

Cycle 2
Body 1 :  0.020709   0.000718   0.000011 |  0.010710   0.000721   0.000014
Body 2 :  0.992907   0.992896   0.039971 | -0.007109  -0.007138   0.019935
Body 3 :  0.020717   0.972888   0.972173 |  0.010727  -0.017153  -0.017876

Cycle 3
Body 1 :  0.031600   0.001625   0.000034 |  0.011072   0.001094   0.000033
Body 2 :  0.983985   0.983910   0.059834 | -0.010735  -0.010835   0.019790
Body 3 :  0.031643   0.953868   0.952235 |  0.011125  -0.020886  -0.021999

Cycle 4
Body 1 :  0.042858   0.002913   0.000081 |  0.011443   0.001481   0.000060
Body 2 :  0.971393   0.971163   0.079509 | -0.014448  -0.014659   0.019561
Body 3 :  0.042981   0.931039   0.928087 |  0.011552  -0.024772  -0.026299

Cycle 5
Body 1 :  0.054492   0.004595   0.000160 |  0.011826   0.001884   0.000097
Body 2 :  0.955030   0.954509   0.098909 | -0.018278  -0.018649   0.019238
Body 3 :  0.054766   0.904225   0.899522 |  0.012018  -0.028857  -0.030829

Cycle 6
Body 1 :  0.066517   0.006691   0.000281 |  0.012224   0.002308   0.000145
Body 2 :  0.934759   0.933760   0.117931 | -0.022265  -0.022849   0.018806
Body 3 :  0.067040   0.873197   0.866280 |  0.012530  -0.033199  -0.035655

Cycle 7
Body 1 :  0.078950   0.009225   0.000456 |  0.012642   0.002759   0.000206
Body 2 :  0.910400   0.908677   0.136456 | -0.026454  -0.027316   0.018244
Body 3 :  0.079856   0.837662   0.828023 |  0.013101  -0.037871  -0.040861

Cycle 8
Body 1 :  0.091815   0.012227   0.000702 |  0.013086   0.003245   0.000284
Body 2 :  0.881722   0.878958   0.154340 | -0.030902  -0.032122   0.017523
Body 3 :  0.093281   0.797239   0.784313 |  0.013749  -0.042975  -0.046559

Cycle 9
Body 1 :  0.105140   0.015737   0.001035 |  0.013564   0.003775   0.000383
Body 2 :  0.848429   0.844216   0.171401 | -0.035684  -0.037362   0.016600
Body 3 :  0.107405   0.751427   0.734579 |  0.014498  -0.048649  -0.052908

Cycle 10
Body 1 :  0.118964   0.019805   0.001481 |  0.014085   0.004362   0.000509
Body 2 :  0.810137   0.803953   0.187408 | -0.040900  -0.043166   0.015414
Body 3 :  0.122346   0.699554   0.678056 |  0.015384  -0.055097  -0.060138

Cycle 11
Body 1 :  0.133337   0.024498   0.002071 |  0.014662   0.005025   0.000672
Body 2 :  0.766343   0.757509   0.202050 | -0.046687  -0.049720   0.013868
Body 3 :  0.138268   0.640690   0.613685 |  0.016460  -0.062633  -0.068603

Cycle 12
Body 1 :  0.148327   0.029907   0.002851 |  0.015317   0.005792   0.000888
Body 2 :  0.716377   0.703998   0.214889 | -0.053246  -0.057302   0.011810
Body 3 :  0.155406   0.573482   0.539941 |  0.017816  -0.071782  -0.078886

Cycle 13
Body 1 :  0.164025   0.036157   0.003887 |  0.016079   0.006709   0.001184
Body 2 :  0.659310   0.642172   0.225282 | -0.060887  -0.066351   0.008976
Body 3 :  0.174112   0.495836   0.454475 |  0.019596  -0.083511  -0.092045

Cycle 14
Body 1 :  0.180564   0.043437   0.005286 |  0.017000   0.007852   0.001613
Body 2 :  0.593807   0.570186   0.232208 | -0.070119  -0.077621   0.004875
Body 3 :  0.194929   0.404136   0.353320 |  0.022038  -0.099890  -0.110265

Cycle 15
Body 1 :  0.198150   0.052049   0.007234 |  0.018171   0.009372   0.002283
Body 2 :  0.517817   0.485100   0.233878 | -0.081861  -0.092550  -0.001535
Body 3 :  0.218605   0.290860   0.228583 |  0.025314  -0.126661  -0.139210

Cycle 16
Body 1 :  0.217126   0.062542   0.010117 |  0.019781   0.011614   0.003484
Body 2 :  0.427899   0.381659   0.226654 | -0.097974  -0.114332  -0.012913
Body 3 :  0.244268   0.131956   0.057562 |  0.026013  -0.191148  -0.202831

Cycle 17
Body 1 :  0.238346   0.076539   0.015221 |  0.022658   0.016380   0.006723
Body 2 :  0.317489   0.248502   0.200967 | -0.122846  -0.151982  -0.038461
Body 3 :  0.075592  -0.559591  -0.487315 | -0.363366  -1.191945  -0.886924

Cycle 18
Body 1 :  0.263123   0.097523   0.026918 |  0.026898   0.025587   0.016672
Body 2 :  0.173428   0.050424   0.112716 | -0.165275  -0.244174  -0.138041
Body 3 : -0.286241  -1.745597  -1.369528 | -0.360299  -1.180066  -0.877501

Cycle 19
Body 1 :  0.270854   0.113045   0.061923 | -0.011436   0.005457   0.053339
Body 2 :  0.199821  -0.093105  -0.208666 |  0.218061  -0.042882  -0.504723
Body 3 : -0.646318  -2.924909  -2.246453 | -0.359856  -1.178559  -0.876350

Cycle 20
Body 1 :  0.258572   0.116046   0.112038 | -0.013129   0.000544   0.046890
Body 2 :  0.426346  -0.111425  -0.681150 |  0.234987   0.006241  -0.440245
Body 3 : -1.006089  -4.103186  -3.122591 | -0.359686  -1.177995  -0.875924
```


=={{header|C#|C sharp}}==
{{trans|D}}

```csharp
using System;
using System.IO;

namespace NBodyProblem {
    class Vector3D {
        public Vector3D(double x, double y, double z) {
            X = x;
            Y = y;
            Z = z;
        }

        public double X { get; }
        public double Y { get; }
        public double Z { get; }

        public double Mod() {
            return Math.Sqrt(X * X + Y * Y + Z * Z);
        }

        public static Vector3D operator +(Vector3D lhs, Vector3D rhs) {
            return new Vector3D(lhs.X + rhs.X, lhs.Y + rhs.Y, lhs.Z + rhs.Z);
        }

        public static Vector3D operator -(Vector3D lhs, Vector3D rhs) {
            return new Vector3D(lhs.X - rhs.X, lhs.Y - rhs.Y, lhs.Z - rhs.Z);
        }

        public static Vector3D operator *(Vector3D lhs, double rhs) {
            return new Vector3D(lhs.X * rhs, lhs.Y * rhs, lhs.Z * rhs);
        }
    }

    class NBody {
        private readonly double gc;
        private readonly int bodies;
        private readonly int timeSteps;
        private readonly double[] masses;
        private readonly Vector3D[] positions;
        private readonly Vector3D[] velocities;
        private readonly Vector3D[] accelerations;

        public NBody(string fileName) {
            string[] lines = File.ReadAllLines(fileName);

            string[] gbt = lines[0].Split();
            gc = double.Parse(gbt[0]);
            bodies = int.Parse(gbt[1]);
            timeSteps = int.Parse(gbt[2]);

            masses = new double[bodies];
            positions = new Vector3D[bodies];
            velocities = new Vector3D[bodies];
            accelerations = new Vector3D[bodies];
            for (int i = 0; i < bodies; ++i) {
                masses[i] = double.Parse(lines[i * 3 + 1]);
                positions[i] = Decompose(lines[i * 3 + 2]);
                velocities[i] = Decompose(lines[i * 3 + 3]);
            }

            Console.WriteLine("Contents of {0}", fileName);
            foreach (string line in lines) {
                Console.WriteLine(line);
            }
            Console.WriteLine();
            Console.Write("Body   :      x          y          z    |");
            Console.WriteLine("     vx         vy         vz");
        }

        public int GetTimeSteps() {
            return timeSteps;
        }

        private Vector3D Decompose(string line) {
            string[] xyz = line.Split();
            double x = double.Parse(xyz[0]);
            double y = double.Parse(xyz[1]);
            double z = double.Parse(xyz[2]);
            return new Vector3D(x, y, z);
        }

        private void ComputeAccelerations() {
            for (int i = 0; i < bodies; ++i) {
                accelerations[i] = new Vector3D(0, 0, 0);
                for (int j = 0; j < bodies; ++j) {
                    if (i != j) {
                        double temp = gc * masses[j] / Math.Pow((positions[i] - positions[j]).Mod(), 3);
                        accelerations[i] = accelerations[i] + (positions[j] - positions[i]) * temp;
                    }
                }
            }
        }

        private void ComputeVelocities() {
            for (int i = 0; i < bodies; ++i) {
                velocities[i] = velocities[i] + accelerations[i];
            }
        }

        private void ComputePositions() {
            for (int i = 0; i < bodies; ++i) {
                positions[i] = positions[i] + velocities[i] + accelerations[i] * 0.5;
            }
        }

        private void ResolveCollisions() {
            for (int i = 0; i < bodies; ++i) {
                for (int j = i + 1; j < bodies; ++j) {
                    if (positions[i].X == positions[j].X
                     && positions[i].Y == positions[j].Y
                     && positions[i].Z == positions[j].Z) {
                        Vector3D temp = velocities[i];
                        velocities[i] = velocities[j];
                        velocities[j] = temp;
                    }
                }
            }
        }

        public void Simulate() {
            ComputeAccelerations();
            ComputePositions();
            ComputeVelocities();
            ResolveCollisions();
        }

        public void PrintResults() {
            for (int i = 0; i < bodies; ++i) {
                Console.WriteLine(
                    "Body {0} : {1,9:F6}  {2,9:F6}  {3,9:F6} | {4,9:F6}  {5,9:F6}  {6,9:F6}",
                    i + 1,
                    positions[i].X, positions[i].Y, positions[i].Z,
                    velocities[i].X, velocities[i].Y, velocities[i].Z
                );
            }
        }
    }

    class Program {
        static void Main(string[] args) {
            NBody nb = new NBody("nbody.txt");

            for (int i = 0; i < nb.GetTimeSteps(); ++i) {
                Console.WriteLine();
                Console.WriteLine("Cycle {0}", i + 1);
                nb.Simulate();
                nb.PrintResults();
            }
        }
    }
}
```

{{out}}

```txt
Contents of nbody.txt
0.01 3 20
1
0 0 0
0.01 0 0
0.1
1 1 0
0 0 0.02
0.001
0 1 1
0.01 -0.01 -0.01

Body   :      x          y          z    |     vx         vy         vz

Cycle 1
Body 1 :  0.010177   0.000179   0.000002 |  0.010354   0.000357   0.000004
Body 2 :  0.998230   0.998232   0.020002 | -0.003539  -0.003536   0.020004
Body 3 :  0.010177   0.988232   0.988055 |  0.010354  -0.013536  -0.013889

Cycle 2
Body 1 :  0.020709   0.000718   0.000011 |  0.010710   0.000721   0.000014
Body 2 :  0.992907   0.992896   0.039971 | -0.007109  -0.007138   0.019935
Body 3 :  0.020717   0.972888   0.972173 |  0.010727  -0.017153  -0.017876

Cycle 3
Body 1 :  0.031600   0.001625   0.000034 |  0.011072   0.001094   0.000033
Body 2 :  0.983985   0.983910   0.059834 | -0.010735  -0.010835   0.019790
Body 3 :  0.031643   0.953868   0.952235 |  0.011125  -0.020886  -0.021999

Cycle 4
Body 1 :  0.042858   0.002913   0.000081 |  0.011443   0.001481   0.000060
Body 2 :  0.971393   0.971163   0.079509 | -0.014448  -0.014659   0.019561
Body 3 :  0.042981   0.931039   0.928087 |  0.011552  -0.024772  -0.026299

Cycle 5
Body 1 :  0.054492   0.004595   0.000160 |  0.011826   0.001884   0.000097
Body 2 :  0.955030   0.954509   0.098909 | -0.018278  -0.018649   0.019238
Body 3 :  0.054766   0.904225   0.899522 |  0.012018  -0.028857  -0.030829

Cycle 6
Body 1 :  0.066517   0.006691   0.000281 |  0.012224   0.002308   0.000145
Body 2 :  0.934759   0.933760   0.117931 | -0.022265  -0.022849   0.018806
Body 3 :  0.067040   0.873197   0.866280 |  0.012530  -0.033199  -0.035655

Cycle 7
Body 1 :  0.078950   0.009225   0.000456 |  0.012642   0.002759   0.000206
Body 2 :  0.910400   0.908677   0.136456 | -0.026454  -0.027316   0.018244
Body 3 :  0.079856   0.837662   0.828023 |  0.013101  -0.037871  -0.040861

Cycle 8
Body 1 :  0.091815   0.012227   0.000702 |  0.013086   0.003245   0.000284
Body 2 :  0.881722   0.878958   0.154340 | -0.030902  -0.032122   0.017523
Body 3 :  0.093281   0.797239   0.784313 |  0.013749  -0.042975  -0.046559

Cycle 9
Body 1 :  0.105140   0.015737   0.001035 |  0.013564   0.003775   0.000383
Body 2 :  0.848429   0.844216   0.171401 | -0.035684  -0.037362   0.016600
Body 3 :  0.107405   0.751427   0.734579 |  0.014498  -0.048649  -0.052908

Cycle 10
Body 1 :  0.118964   0.019805   0.001481 |  0.014085   0.004362   0.000509
Body 2 :  0.810137   0.803953   0.187408 | -0.040900  -0.043166   0.015414
Body 3 :  0.122346   0.699554   0.678056 |  0.015384  -0.055097  -0.060138

Cycle 11
Body 1 :  0.133337   0.024498   0.002071 |  0.014662   0.005025   0.000672
Body 2 :  0.766343   0.757509   0.202050 | -0.046687  -0.049720   0.013868
Body 3 :  0.138268   0.640690   0.613685 |  0.016460  -0.062633  -0.068603

Cycle 12
Body 1 :  0.148327   0.029907   0.002851 |  0.015317   0.005792   0.000888
Body 2 :  0.716377   0.703998   0.214889 | -0.053246  -0.057302   0.011810
Body 3 :  0.155406   0.573482   0.539941 |  0.017816  -0.071782  -0.078886

Cycle 13
Body 1 :  0.164025   0.036157   0.003887 |  0.016079   0.006709   0.001184
Body 2 :  0.659310   0.642172   0.225282 | -0.060887  -0.066351   0.008976
Body 3 :  0.174112   0.495836   0.454475 |  0.019596  -0.083511  -0.092045

Cycle 14
Body 1 :  0.180564   0.043437   0.005286 |  0.017000   0.007852   0.001613
Body 2 :  0.593807   0.570186   0.232208 | -0.070119  -0.077621   0.004875
Body 3 :  0.194929   0.404136   0.353320 |  0.022038  -0.099890  -0.110265

Cycle 15
Body 1 :  0.198150   0.052049   0.007234 |  0.018171   0.009372   0.002283
Body 2 :  0.517817   0.485100   0.233878 | -0.081861  -0.092550  -0.001535
Body 3 :  0.218605   0.290860   0.228583 |  0.025314  -0.126661  -0.139210

Cycle 16
Body 1 :  0.217126   0.062542   0.010117 |  0.019781   0.011614   0.003484
Body 2 :  0.427899   0.381659   0.226654 | -0.097974  -0.114332  -0.012913
Body 3 :  0.244268   0.131956   0.057562 |  0.026013  -0.191148  -0.202831

Cycle 17
Body 1 :  0.238346   0.076539   0.015221 |  0.022658   0.016380   0.006723
Body 2 :  0.317489   0.248502   0.200967 | -0.122846  -0.151982  -0.038461
Body 3 :  0.075592  -0.559591  -0.487315 | -0.363366  -1.191945  -0.886924

Cycle 18
Body 1 :  0.263123   0.097523   0.026918 |  0.026898   0.025587   0.016672
Body 2 :  0.173428   0.050424   0.112716 | -0.165275  -0.244174  -0.138041
Body 3 : -0.286241  -1.745597  -1.369528 | -0.360299  -1.180066  -0.877501

Cycle 19
Body 1 :  0.270854   0.113045   0.061923 | -0.011436   0.005457   0.053339
Body 2 :  0.199821  -0.093105  -0.208666 |  0.218061  -0.042882  -0.504723
Body 3 : -0.646318  -2.924909  -2.246453 | -0.359856  -1.178559  -0.876350

Cycle 20
Body 1 :  0.258572   0.116046   0.112038 | -0.013129   0.000544   0.046890
Body 2 :  0.426346  -0.111425  -0.681150 |  0.234987   0.006241  -0.440245
Body 3 : -1.006089  -4.103186  -3.122591 | -0.359686  -1.177995  -0.875924
```



## D

{{trans|Kotlin}}

```D
import std.algorithm;
import std.array;
import std.conv;
import std.file;
import std.math : sqrt;
import std.stdio;
import std.string;

/// End of (type) recursion
void unpack(T)(T t) { /* empty */ }

/// Useful for unpacking tuples. Beware of assigning mutable buffers, for the u's will alias the original t's.
void unpack(T, U ...)(T t, ref U u) in {
    assert(t.length == u.length);
} body {
    if (t.length > 0) {
        u[0] = t[0];
        unpack(t[1..$], u[1..$]);
    }
}

unittest {
    auto t = ["one", "two"];
    string f, s;
    unpack(t, f, s);
    assert(f=="one");
    assert(s=="two");
}

alias FLOAT = real;

struct Vector3D {
    FLOAT x, y, z;

    this(FLOAT x, FLOAT y, FLOAT z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    auto opBinary(string op : "+")(Vector3D v) {
        return Vector3D(x + v.x, y + v.y, z + v.z);
    }
    auto opOpAssign(string op : "+")(Vector3D v) {
        x += v.x;
        y += v.y;
        z += v.z;
        return this;
    }

    auto opBinary(string op : "-")(Vector3D v) {
        return Vector3D(x - v.x, y - v.y, z - v.z);
    }

    auto opBinary(string op : "*")(FLOAT s) {
        return Vector3D(s*x, s*y, s*z);
    }

    auto mod() {
        return sqrt(x*x + y*y + z*z);
    }
}

auto origin = Vector3D(0, 0, 0);

struct NBody {

    FLOAT gc;
    int bodies;
    int timeSteps;
    FLOAT[] masses;
    Vector3D[] positions;
    Vector3D[] velocities;
    Vector3D[] accelerations;

    this(string fileName) {
        auto lines = File(fileName).byLine();
        char[] g, b, t;
        lines.front.split().unpack(g, b, t);
        gc = g.to!FLOAT;
        bodies = b.to!int;
        timeSteps = t.to!int;
        lines.popFront; // Must come after g, b and t have been processed.

        masses.length = bodies;
        positions.length = bodies;
        positions[] = origin;
        velocities.length = bodies;
        velocities[] = origin;
        accelerations.length = bodies;
        accelerations[] = origin;
        foreach (i; 0..bodies) {
            masses[i] = lines.front.to!FLOAT;
            lines.popFront;
            positions[i] = decompose(lines.front);
            lines.popFront;
            velocities[i] = decompose(lines.front);
            lines.popFront;
        }
        writeln("Contents of $fileName");
        writeln(readText(fileName));
        writeln("Body   :      x          y          z    |");
        writeln("     vx         vy         vz");
    }

    private Vector3D decompose(char[] line) {
        FLOAT x, y, z;
        line.split().map!(to!FLOAT).unpack(x, y, z);
        return Vector3D(x, y, z);
    }

    private void computeAccelerations() {
        foreach (i; 0..bodies) {
            accelerations[i] = origin;
            foreach (j; 0..bodies) {
                if (i != j) {
                    auto temp = gc * masses[j] / (positions[i] - positions[j]).mod^^3;
                    accelerations[i] += (positions[j] - positions[i]) * temp;
                }
            }
        }
    }

    private void computePositions() {
        foreach (i; 0..bodies) {
            positions[i] += velocities[i] + accelerations[i] * 0.5;
        }
    }

    private void computeVelocities() {
        foreach(i; 0..bodies) velocities[i] += accelerations[i];
    }

    private void resolveCollisions() {
        foreach(i; 0..bodies) {
            foreach (j; i+1..bodies) {
                if (positions[i].x == positions[j].x &&
                    positions[i].y == positions[j].y &&
                    positions[i].z == positions[j].z) {
                    swap(velocities[i], velocities[j]);
                }
            }
        }
    }

    void simulate() {
        computeAccelerations();
        computePositions();
        computeVelocities();
        resolveCollisions();
    }

    void printResults() {
        auto fmt = "Body %d : % 8.6f  % 8.6f  % 8.6f | % 8.6f  % 8.6f  % 8.6f";
        foreach(i; 0..bodies) {
            writefln(fmt,
                i + 1,
                positions[i].x,
                positions[i].y,
                positions[i].z,
                velocities[i].x,
                velocities[i].y,
                velocities[i].z
            );
        }
    }
}

void main() {
    auto fileName = "nbody.txt";
    auto nb = NBody(fileName);
    foreach (i; 1..nb.timeSteps+1) {
        writeln("\nCycle ", i);
        nb.simulate();
        nb.printResults();
    }
}
```

{{out}}

```txt
Contents of nbody.txt
0.01 3 20
1
0 0 0
0.01 0 0
0.1
1 1 0
0 0 0.02
0.001
0 1 1
0.01 -0.01 -0.01

Body   :      x          y          z    |
     vx         vy         vz

Cycle 1
Body 1 :  0.010176   0.000178   0.000001 |  0.010353   0.000357   0.000003
Body 2 :  0.998230   0.998232   0.020001 | -0.003539  -0.003535   0.020003
Body 3 :  0.010176   0.988232   0.988055 |  0.010353  -0.013535  -0.013889

Cycle 2
Body 1 :  0.020708   0.000717   0.000010 |  0.010710   0.000720   0.000014
Body 2 :  0.992907   0.992896   0.039971 | -0.007108  -0.007137   0.019935
Body 3 :  0.020717   0.972888   0.972173 |  0.010726  -0.017153  -0.017876

Cycle 3
Body 1 :  0.031599   0.001625   0.000034 |  0.011072   0.001094   0.000033
Body 2 :  0.983985   0.983910   0.059833 | -0.010735  -0.010834   0.019789
Body 3 :  0.031642   0.953868   0.952235 |  0.011124  -0.020886  -0.021998

Cycle 4
Body 1 :  0.042857   0.002912   0.000081 |  0.011443   0.001480   0.000060
Body 2 :  0.971393   0.971163   0.079509 | -0.014447  -0.014659   0.019561
Body 3 :  0.042981   0.931039   0.928087 |  0.011552  -0.024771  -0.026298

Cycle 5
Body 1 :  0.054492   0.004594   0.000159 |  0.011825   0.001883   0.000097
Body 2 :  0.955030   0.954509   0.098908 | -0.018278  -0.018648   0.019238
Body 3 :  0.054766   0.904225   0.899522 |  0.012017  -0.028856  -0.030829

Cycle 6
Body 1 :  0.066517   0.006690   0.000280 |  0.012223   0.002308   0.000145
Body 2 :  0.934759   0.933760   0.117931 | -0.022264  -0.022849   0.018806
Body 3 :  0.067040   0.873197   0.866280 |  0.012529  -0.033198  -0.035655

Cycle 7
Body 1 :  0.078950   0.009224   0.000456 |  0.012642   0.002759   0.000206
Body 2 :  0.910400   0.908677   0.136456 | -0.026453  -0.027316   0.018244
Body 3 :  0.079855   0.837662   0.828023 |  0.013101  -0.037871  -0.040860

Cycle 8
Body 1 :  0.091814   0.012226   0.000701 |  0.013086   0.003245   0.000284
Body 2 :  0.881722   0.878958   0.154340 | -0.030902  -0.032121   0.017523
Body 3 :  0.093281   0.797239   0.784313 |  0.013749  -0.042975  -0.046559

Cycle 9
Body 1 :  0.105140   0.015736   0.001035 |  0.013563   0.003774   0.000382
Body 2 :  0.848429   0.844216   0.171401 | -0.035684  -0.037361   0.016600
Body 3 :  0.107405   0.751427   0.734579 |  0.014498  -0.048649  -0.052908

Cycle 10
Body 1 :  0.118964   0.019805   0.001481 |  0.014084   0.004361   0.000508
Body 2 :  0.810137   0.803953   0.187408 | -0.040899  -0.043166   0.015414
Body 3 :  0.122346   0.699554   0.678056 |  0.015383  -0.055096  -0.060138

Cycle 11
Body 1 :  0.133337   0.024498   0.002071 |  0.014662   0.005024   0.000671
Body 2 :  0.766343   0.757509   0.202050 | -0.046686  -0.049720   0.013868
Body 3 :  0.138268   0.640690   0.613685 |  0.016460  -0.062632  -0.068602

Cycle 12
Body 1 :  0.148327   0.029906   0.002851 |  0.015316   0.005791   0.000887
Body 2 :  0.716377   0.703998   0.214889 | -0.053245  -0.057301   0.011810
Body 3 :  0.155406   0.573482   0.539941 |  0.017815  -0.071781  -0.078886

Cycle 13
Body 1 :  0.164025   0.036157   0.003887 |  0.016079   0.006708   0.001184
Body 2 :  0.659310   0.642172   0.225282 | -0.060886  -0.066350   0.008976
Body 3 :  0.174112   0.495836   0.454475 |  0.019596  -0.083510  -0.092044

Cycle 14
Body 1 :  0.180564   0.043437   0.005285 |  0.016999   0.007852   0.001612
Body 2 :  0.593807   0.570186   0.232208 | -0.070119  -0.077621   0.004875
Body 3 :  0.194929   0.404136   0.353320 |  0.022038  -0.099889  -0.110265

Cycle 15
Body 1 :  0.198150   0.052049   0.007233 |  0.018170   0.009371   0.002282
Body 2 :  0.517817   0.485100   0.233878 | -0.081861  -0.092550  -0.001534
Body 3 :  0.218605   0.290860   0.228583 |  0.025313  -0.126661  -0.139210

Cycle 16
Body 1 :  0.217126   0.062542   0.010117 |  0.019781   0.011614   0.003484
Body 2 :  0.427899   0.381659   0.226654 | -0.097974  -0.114332  -0.012913
Body 3 :  0.244268   0.131956   0.057562 |  0.026012  -0.191148  -0.202831

Cycle 17
Body 1 :  0.238346   0.076539   0.015220 |  0.022658   0.016380   0.006723
Body 2 :  0.317489   0.248502   0.200967 | -0.122846  -0.151982  -0.038460
Body 3 :  0.075591  -0.559591  -0.487315 | -0.363366  -1.191945  -0.886924

Cycle 18
Body 1 :  0.263123   0.097523   0.026918 |  0.026897   0.025587   0.016671
Body 2 :  0.173428   0.050423   0.112716 | -0.165275  -0.244174  -0.138041
Body 3 : -0.286241  -1.745597  -1.369528 | -0.360299  -1.180066  -0.877501

Cycle 19
Body 1 :  0.270854   0.113045   0.061923 | -0.011436   0.005456   0.053338
Body 2 :  0.199821  -0.093104  -0.208666 |  0.218061  -0.042882  -0.504723
Body 3 : -0.646318  -2.924909  -2.246453 | -0.359856  -1.178559  -0.876350

Cycle 20
Body 1 :  0.258572   0.116046   0.112038 | -0.013129   0.000543   0.046890
Body 2 :  0.426346  -0.111425  -0.681150 |  0.234987   0.006240  -0.440245
Body 3 : -1.006089  -4.103186  -3.122591 | -0.359686  -1.177995  -0.875924
```



## Fortran


### Interpretation

As stated, the task involves a general rendition of <math>F = ma</math>, in the sense that a mass is being accelerated (that is, its position <math> \vec r </math> is changing, and the second differential of position against time is the acceleration) and therefore there is a force, but the cause and nature of the acceleration is unstated. Then there is mention of masses mutually interacting according to Newton's law of gravitation, so the formula becomes for each ''i'' and ''j'' not equal
: <math>\vec d = \vec r_{i} - \vec r_{j}</math>
: <math>\vec F_{ij} = - G \frac{m_i m_j}{|\vec d|^2} \frac{\vec d}{|\vec d|}</math>
The bodies are presumed to be able to move freely (not sliding on a wire or across a surface, etc.) so at any position the forces can be calculated and thereby the accelerations, but, only via a function of position. What is desired however is to determine the positions as a function of time. Analysis of the formulae combine constraints that lead to the conclusion that the paths are conic sections: a circle, ellipse, parabola or hyperbola. Thus, for a small mass ''m'' in a circular orbit around a large mass ''M'', the force required to bend the path into a circle is directed towards the circle's centre and is given by
: <math> F = m \frac{v^2}{r} = G \frac{M m}{r^2}</math> 
where ''r'' is the radius of the orbit and ''v'' its orbital velocity. This assumes that the gravitational mass and inertial masses are the same - experimentally tested by Newton, and to great accuracy by Eötvös ''et seq''. For a constant-velocity circular orbit, the time for one orbit to complete is 
: <math> T = \frac{2 \pi r}{v} </math>
The value of ''m'' can be cancelled from both sides to give a limiting case where ''m'' is zero, a "test mass" (so this is almost a one-body problem) and
: <math> T = 2 \pi \sqrt{ \frac{r^3}{GM}}</math>
A fuller analysis allowing for elliptical orbits finds that 
: <math> T = 2 \pi \sqrt{ \frac{r^3}{G(M + m)}}</math>
In the MKS system, G = 6·6742E-11 so for a one-second orbit around a one kilogram mass, r = 0·000119 metres, a tenth of a millimetre! A sphere of that radius has volume 7E-12 cubic metres so the density would have to be at least 1.4E+11 KG/cubic metre; osmium manages 2.25E+4, neutronium  4E+17. Alternatively, with two one-kilogram masses orbiting their centre of mass with r = 1 metre, T = 6·3 days... The force between the two spheres is just G, 6·6742E-11 Newtons. Preventing such a languid orbit from being disrupted by electrostatic forces (due to cosmic rays, radioactive disintegration, etc.) will be difficult. Determining G via an experiment in which two ''known'' masses orbit each other does not look promising.

For more than two bodies the possible behaviours become numerous while the constraints are too few. Notably, bodies can pass within infinitesimal distances of each other and thereby suffer disruptive accelerations. For three bodies certain arrangements remain easily solved: one body having zero mass, or a symmetric arrangement of all three may provide sufficient constraints such as equal masses at the points of an equilateral triangle with equal velocities. For more numerous collections, analysis is abandoned in favour of computation.


### Numerical approach

The basic step is for each body, sum the forces on it exerted by every other body, then apply F = ma and divide by its mass to determine its acceleration. The only shortcut lies in noting that the force of body A on body B is exactly that of body B on body A (in the reverse direction along the vector joining them) so that the task can be halved. ''In Newtonian physics, there is no time lag across distance.'' Then, wrongly supposing those accelerations constant for some small time interval, calculate how they would affect the velocities and the positions over that time interval, using a step size that is small enough for this to be approximately true... Too small a step size not only means a lengthy calculation but introduces error by accumulation as in the formation of the sum of a very numerous collection of values.

The Fortran programme was written in the early 1990s and so employs F77. A F90 compiler was available on the IBM mainframe, but it must have been an early version because it was riddled with errors. A colleague tried its QUADRUPLE PRECISION facility, and found that it crashed the operating system! In-line comments were unavailable, and the twenty-four line IBM3278 display screen discouraged expending screen space on commentary, especially because they were non-scrolling. It was also an exploratory programme, so documenting details that were a variation and might soon be removed was a further discouragement. In other words, I now find it difficult to follow the code. (Stop laughing!) The initial effort was prompted by an article in Scientific American that listed the masses, positions and velocities of the twenty nearest stars and invited calculations. It soon became apparent that the stars were not gravitationally bound to each other, as they simply drifted apart along nearly straight lines.

The other area of interest was the performance of various methods for numerical approximation. The calculation effort is assessed on the basis of the number of evaluations of the differential equation required for each step: outside of textbook examples this is usually a large effort. Here for example it is proportional to the square of the number of bodies, and a globular cluster may contain not just hundreds but millions of stars... Thus, the first-order Euler method requires one calculation of accelerations per step, the second-order method two, and the Runge-Kutta method four. Only if the step size could be increased more than proportionally would higher-order methods be worthwhile - provided that accuracy was maintained! Error bounds can be calculated, both for an individual step and for the result after a sequence of steps, via the usual analysis for a polynomial solution function (and if it were a polynomial then numerical methods would not be needed) and as usual produces results that rely on there being an upper bound to a suitably high differential of the solution function over the interval. But in this problem, there is no such bound. Bodies, as points, can approach arbitrarily closely and the resulting accelerations are arbitrarily high, being 1/d<sup>2</sup> as d approaches zero. Thus, no assurances are available, and a common method is to run a calculation once with a given step size, then again with half the step size, at a painful cost. The irresponsible make a guess at a suitable step size and worry no further.

All the straightforward methods attempt to assess the conditions over the step, perhaps briefly as with the Euler method using the conditions only at the start, whereas the classic fourth-order Runge-Kutta method first probes half a step forwards, uses those results to probe for a full step, then combines all results to make the full step. Such probing requires many evaluations. Rather than forgetting previous positions so painfully computed, the predictor-corrector methods instead rely on the recent past history as being a good indication of the near future - on the supposition of no step-discontinuities. This problem is slightly more difficult because it is second order (as for acceleration to velocity to position) rather than the first order (say, velocity to position) problems discussed in texts. Specifically, the predictor part is to fit a polynomial to the past few accelerations (e.g. at times t-2h, t-h and t) and integrate to obtain a provisional position for the ''end'' of the current time step at t+h. Evaluate the acceleration there and use it as the new value to perform the actual advance to the end of the time step, the corrector part. Since this is now the new position (and will differ from the provisional position based on polynomial extrapolation), recalculate the acceleration there to maintain coherence between position and acceleration. There are thus just two evaluations per step, and this applies even if more values are retained for higher-order extrapolation and integration methods. 

This could be reduced to one evaluation per step: as before use a polynomial fitted to the past accelerations to enable integration so as to determine the position at t+h. Calculate the acceleration for that position (at time t+h) to complete the step, ready to start the next. A sense of unease should arise at this, made clear by asking "Why bother with the corrector at all?" - but that would mean not involving the actual differential equation! The predictor step involves extrapolating a polynomial fit over t-2h, t-h and t (all being accepted solutions for the differential equation), to t+h but the predicted acceleration at time t+h is likely to match the differential equation's value at the new position determined from it only if the solution is a simple polynomial. Put another way, the second-order method employs the differential equation twice (at both ends of the step), and does much better than the first-order method with one evaluation.

Two temptations are to be avoided: using ever-higher order polynomials for extrapolation and integration amounts to saying that the differential equation is a polynomial, and its is not - no polynomial has a vertical or horizontal asymptote and gravitation has both, so another possibility would be to try fitting the values with other functions (perhaps with negative powers: Laurent series?), not just polynomials. Secondly, avoid the "iteration heresy" (F.S. Acton, ''Numerical Methods that Work (Usually)'') whereby there is a temptation to iterate the predictor-corrector cycle for each step until it converges. Alas, it will converge to the solution of a difference equation, and this will only be the solution of the ''differential'' equation after the step size is reduced to zero. Further, such iteration will only re-evaluate the accelerations at almost the same positions each time, leaving the rest of the step interval uninspected. Traversing that step via multiple smaller steps would be a much better use for all those extra evaluations, spreading them evenly.

The simple methods (Euler, second-order, Runge-Kutta) can easily accommodate changes to the step size, but guidance for these changes is not easily obtained. The Milne predictor-corrector method is much messier but contains an inbuilt assessment of the possibilities for changing the step size: at the new position at the end of the step, compare the predicted value for the acceleration with the final calculated value. With the "extrapolation to zero step size" methods of Gragg, Bulirsch, and Stoer, arithmetical analysis of the effect of different step sizes is central to its workings. With calculus the purpose is the analysis of the limiting behaviour as a step is reduced to zero with a view to obtaining results spanning some region of interest that may be ±infinity. Only with the GBS approach is there a corresponding analysis, an arithmetical analysis. It does not concern itself with the nature of the functions but only with the limited-precision inspection of values obtained via limited-precision evaluation.

On escalating to the Milne Predictor-Corrector method, there arose a large increase in administration, especially when the time step was to be doubled or halved. Previous methods had involved just a little juggling but now the juggling was so variable that introducing a multi-item stash with an allocation and freeing scheme reduced the strain. Under consideration were questions such as how to compare two vectors for being adequately close (as in deciding whether to change the step size) when their numerical value might vary over a wide range (astronomical units, or MKS?) yet one or both may be zero too, and whether to stick with halving/doubling or consider halving/tripling so that if (say) 2 was too small and 4 too big, intermediate values such as 3 could be reached, and whether this would be worth the floundering about.


### Organisation of arrays

Aside from the details of the calculations and their method, organising the data structure is important. Accessing an array element is time-consuming, so a particular ploy was to de-reference the arrays: from three dimensions to two (via NEWT) and from two to one (via NEWTON) so that the indexing of the time-consuming computation would be simplified. In other words, the storage for X (position), V (velocity) and A (acceleration) are all 100 (to allow for N bodies) by 3 (for x, y, z components) by 14, this last being sufficient multiple sets of data for MILNE to juggle. It uses Runge-Kutta to initialise its march then proceeds, possibly doubling its step size (so discarding alternate saved historic position sets, thus needing to have eight so that it would be able to retain four) or halving its step size. Notably, for a circular orbit, MILNE developed coefficients equivalent to the first few terms of the series expansion of sine. From F90 there appear arrangements for manipulating arrays, and especially the FOR ALL statement, but their exact behaviour is poorly described. For instance, if the FOR ALL statement were to produce results in a work area then copy that to the destination area, performance may be impeded compared to the update-in-place of the straightforward statements due to the extra data transfer involved. 

The arrays are dimensioned as 100,3,14 rather than say 14,3,100 because Fortran orders array elements so that the first index varies most rapidly for adjacent elements in storage - some languages are the other way around and I have never seen an explanation for why this choice was made for Fortran, as it is unhelpful for matrix arithmetic. Thus, element (i,j,k) is followed by element (i + 1,j,k). Accordingly, as NEWTON works along the N bodies, the values for consecutive bodies are adjacent. It is presented with separate X, Y, Z arrays even though these are actually parts of the same big array, so that it may use one-dimensional array indexing (and with the same index for all) rather than the much slower two-dimensional indexing. However, the storage locations of these arrays have fixed relationships with each other. Specifically, each are one hundred elements apart so that Y(i) is one hundred elements along from X(i), and Z(i) another hundred along from Y(i). Thus, there could instead be an array XYZ of 300 elements with the code referencing XYZ(i) to obtain X(i), XYZ(i + 100) for Y(i), and XYZ(i + 200) for Z(i). This would have the obvious advantage of maintaining one-dimensional indexing, and gain the possibility that the compiler might generate code that uses one index register for all the accesses rather than three. This can be achieved for "free" - suppose the determination of the effective address (in memory of a datum) is of the form (base address of XYZ) + (index register: the offset). Rather than calculating offsets from (i) and (i + 100), and (i + 200) the equivalent would be to use an offset of (i) every time, but with (XYZ) and (XYZ + 100) and (XYZ + 200), these all being constants.

This notion could also be extended to the 14 sets of 100 triples. In other words, any multi-dimensional array could be considered as equivalent to a single-dimension array of the same size, and indeed could be made so via an EQUIVALENCE statement. Thus, the notational and organisational conveniences of multi-dimensional arrays could be retained, with the use of single indexing reserved for places of desperate need for speed. However, in the absence of a PARAMETER statement to identify the various constants, any adjustment to the size and shape of the array will be difficult and simple mistakes will often be missed. Serious computational programmes often involve large and complex formulae, so the task is not small. Depending on one's ingenuity, there are many possible schemes to choose amongst and a great deal of time can vanish, possibly greater than any recovered by faster running... 

All of this could, and indeed should, be done via the compiler's analysis rather than have a human disappear down a rabbit hole. First Fortran (1958) engaged in intensive analysis, guided by a Monte Carlo simulation possibly assisted by the programmer supplying hints via the FREQUENCY statement and produced surprisingly cunning code - in part because the (debugged) compiler would engage in complex arithmetic that a human programmer would avoid due to the risk of making a mistake. Subsequent compilers are often praised, but inspection of the code is less impressive, and the task is not well done. For instance, simple changes to the source file result in faster-running programmes, and these changes should not have made any difference. Thus, NEWTON puts values from arrays into simple variables rather than reference the same array element for each usage: the compiler should be able to notice this. There is certainly talk about "invariant expressions" and the like, but, the run times ''are'' different. It is also possible that the code produced by the "optimising" compiler runs slower than that without the "optimising" active, despite the longer compile time. This is not just me: the British Meteorological Office has admitted to similar experiences, during a job interview.

All calculations are in double precision. Test runs with near-approach paths demonstrated the need, even though the constant of gravitation is known to well less than single-precision accuracy. Startling plots resulted, because as a body approaches a mass its velocity builds up to a high value and thus, covering a lot of distance near to the mass in the same time step means that curvature is missed. Further, the build up from a small velocity to high velocities during the approach must be matched by their reduction during the departure, but limited precision means that the details of the low velocity have been lost to truncation/rounding while the number was large and so the departure path will not be the mirror of the approach path. For example, imagine that a single-precision  variable holds the value of pi, then add 1, 10, 100, 1000, 10000, 100000, followed by subtracting 100000, 10000, ... , 1. The value of pi will have been badly damaged. Only in double precision would that single precision accuracy have been maintained.


### Source

I have left the source file pretty much as it was, so F77 rules. This was written in part to exercise the graph-plotting facilities, which served a number of output devices (screen to paper) but in a page-only mode: no moving pictures. Rather than attempt to develop a library of plot assistance routines, for exploration it was easier to incorporate (and possibly fiddle) the most helpful source from other programmes - the Fortran compiler ran at about 50,000 lines/minute, and also I'd had odd experiences with the linkage loader. Those plotting routines are long gone, and there are no similar routines to hand now so they have here been made into dummy routines. Variable DIVE enables runs with plotting deactivated so that text output could be concentrated upon, and also suppressed tedious requests to name the plotting device.

There are also slight changes - the IBM MVS/XA with TSO system did not support the open-file-by-name protocol, so the source now contains an <code>OPEN (IN,FILE="TCL.dat")</code> to read the parameters. During a TSO session, one would assign a named file to a unit number (like a DD statement in JCL for batch jobs) and then run the programme as often as one wished. Similarly, as scrolling screens are now available, there are some additional output statements such as a confirmatory display of what was read from the input file. Previously, one would begrudge every output as it used up one more of the available twenty-three lines, each of seventy-nine characters. The IBM3270 (''et al'') display screens divided the screen space into display fields, each starting with a control field (stating input allowed or not, digits only, may be skipped, bright/dim, etc.) that took up a character space. If when viewing the next screen's output one wished to refer back to previous output, it was too late, there was no scrolling back... One could write helpful output to a text file, but, there was no provision for multiple "windows" on the screen to view the graph and the text together, and not much space anyway even if there were. Typically, one would hit the enter key until the "READY" was close to the bottom of the screen then start the run. This way the resulting output would spill over to a fresh screen and so enable as much as possible to be seen together. To assist in this, there was a facility to specify IST and LST, the first and last body for which output was to be shown, here set to 1 and 3 for the specific three-body example being run.

More seriously, later Fortran has additional routines, and confusion can arise: thus subroutine FREE was renamed to SFREE, and array INDEX renamed to INDEXS, though calling it SINDEX was the first idea - but that would require additional declarations so as to keep it integer. The habit of not bothering with minor declarations lingers on with exploratory programmes...

```Fortran

      SUBROUTINE PLOTS(DX,DY,DEVICE)
       INTEGER DEVICE(10)
       COMMON LINPR
        WRITE (LINPR,*) "PlotS:",DX,DY,DEVICE
      END
      SUBROUTINE PLOT(X,Y,IHIC)
       COMMON LINPR
        WRITE (LINPR,*) "Plot:",X,Y,IHIC
      END
      SUBROUTINE FACTOR(F)
       COMMON LINPR
        WRITE (LINPR,*) "PlotFactor:",F
      END
      SUBROUTINE SYMBOL(X,Y,H,T,A,N)
       INTEGER T(1)
       COMMON LINPR
        WRITE (LINPR,*) "PlotSymbol:",X,Y,H,T,A,N
      END
      SUBROUTINE NUMBER(X,Y,H,V,A,N)
        COMMON LINPR
        WRITE (LINPR,*) "PlotNumber:",X,Y,H,V,A,N
      END
C
### ============
The above suppresses attempts to access a long-lost plotting system
c      FUNCTION CPUTIM(I)
c        CALL INTVAL(-3,IWASTE)
c        CPUTIM = FLOAT(IWASTE)/1000
c        IF (I .EQ. 0) CALL STIME
c        RETURN
c      END
      LOGICAL FUNCTION PRANGE(X1,X2,Y1,Y2)
       LOGICAL ROTATE,DIVE
       INTEGER DEVICE(10),CARDS,BLANK
       COMMON /PLOTIT/ ROTATE,DIVE,DEVICE,BLOAT
       COMMON LINPR,CARDS
       DATA BLANK/'    '/
        PRANGE = DIVE
        IF (DIVE) RETURN
        IF (DEVICE(1).NE.BLANK) GO TO 10
        WRITE (LINPR,1)
    1   FORMAT (' Name your output device (TEK618,GDDM78,..)')
        READ (CARDS,2) DEVICE(1),DEVICE(2)
    2   FORMAT (2A4)
   10   DEVICE(3) = 0
        DEVICE(4) = 4*10
        DX = X2 - X1
        DY = Y2 - Y1
        IF (.NOT.ROTATE) CALL PLOTS(DX,DY,DEVICE)
        IF (     ROTATE) CALL PLOTS(DY,DX,DEVICE)
        PRANGE = DEVICE(3).EQ.0
        IF (PRANGE) GO TO 100
        WRITE (LINPR,11) (DEVICE(I),I = 1,4)
   11   FORMAT (' Muckup.',2A4,2I9)
       RETURN
  100   CALL PLOT(0.0,0.0,3)
        D = AMAX1(DX,DY)
        IF (     ROTATE) CALL FACTOR(BLOAT)
        IF (.NOT.ROTATE) CALL PLOT(-X1,-Y1,-3)
        IF (     ROTATE) CALL PLOT( Y2,-X1,-3)
       RETURN
      END
      SUBROUTINE PLOTXY(X,Y,I)
       LOGICAL ROTATE,DIVE
       COMMON /PLOTIT/ ROTATE,DIVE
C (x,y)*(0,1) = (-y,x), a 90-degree rotation.
        IF (DIVE) RETURN
        IF (.NOT.ROTATE) CALL PLOT( X,Y,I)
        IF (     ROTATE) CALL PLOT(-Y,X,I)
       RETURN
      END
      SUBROUTINE SYMBXY(X,Y,H,T,A,N)
       LOGICAL ROTATE,DIVE
       INTEGER T(1)
       COMMON /PLOTIT/ ROTATE,DIVE
        IF (DIVE) RETURN
        IF (.NOT.ROTATE) CALL SYMBOL(X,Y,H,T,A,N)
        IF (     ROTATE) CALL SYMBOL(-Y,X,H,T,A + 90,N)
       RETURN
      END
      SUBROUTINE NUMBXY(X,Y,H,V,A,N)
       LOGICAL ROTATE,DIVE
       COMMON /PLOTIT/ ROTATE,DIVE
        IF (DIVE) RETURN
        IF (.NOT.ROTATE) CALL NUMBER(X,Y,H,V,A,N)
        IF (     ROTATE) CALL NUMBER(-Y,X,H,V,A + 90,N)
       RETURN
      END
      SUBROUTINE DOBOX(X1,Y1,X2,Y2)
        CALL PLOTXY(X1,Y1,3)
        CALL PLOTXY(X2,Y1,2)
        CALL PLOTXY(X2,Y2,2)
        CALL PLOTXY(X1,Y2,2)
        CALL PLOTXY(X1,Y1,2)
       RETURN
      END
C
### ======
Above are routines for producing plots.
      SUBROUTINE SWAPI(I,J)
        IT = I
        I = J
        J = IT
       RETURN
      END
      SUBROUTINE NEWTON(X,Y,Z,AX,AY,AZ,M,N)
       IMPLICIT REAL*8 (A-H,O-Z)
       REAL*8 X(N),Y(N),Z(N),AX(N),AY(N),AZ(N),M(N)
       REAL*8 MI,MJ
       COMMON /CONST/ G,CLOSE
        DO 1 I = 1,N
          AX(I) = 0.0
          AY(I) = 0.0
    1     AZ(I) = 0.0
        NL1 = N - 1
        DO 3 I = 1,NL1
          AXI = AX(I)
          AYI = AY(I)
          AZI = AZ(I)
          MI = M(I)
          XI = X(I)
          YI = Y(I)
          ZI = Z(I)
          J = I + 1
          DO 2 J = J,N
            MJ = M(J)
            DX = X(J) - XI
            DY = Y(J) - YI
            DZ = Z(J) - ZI
            D2 = DZ*DZ + DY*DY + DX*DX
            IF (D2 .LT. CLOSE) CLOSE = D2
            F = G/(D2*DSQRT(D2))
            AIJ = F*DX
            AXI = AXI + MJ*AIJ
            AX(J) = AX(J) - MI*AIJ
            AIJ = F*DY
            AYI = AYI + MJ*AIJ
            AY(J) = AY(J) - MI*AIJ
            AIJ = F*DZ
            AZI = AZI + MJ*AIJ
    2       AZ(J) = AZ(J) - MI*AIJ
          AX(I) = AXI
          AY(I) = AYI
    3     AZ(I) = AZI
       RETURN
      END
      SUBROUTINE NEWT(X,A,N)
       IMPLICIT REAL*8 (A-H,O-Z)
       REAL*8 X(100,3),A(100,3),M(100)
       COMMON /CONST/ G,CLOSE,M
        CALL NEWTON(X(1,1),X(1,2),X(1,3),A(1,1),A(1,2),A(1,3),M,N)
       RETURN
      END
      SUBROUTINE PROBE(X,X2,V,A,N,DT)
C Looks ahead one time step.
       IMPLICIT REAL*8 (A-H,O-Z)
       REAL*8 X(100,3),V(100,3),A(100,3)
       REAL*8 X2(100,3)
        DO 1 I = 1,N
          DO 1 J = 1,3
    1       X2(I,J) = X(I,J) + (V(I,J) + 0.5*A(I,J)*DT)*DT
       RETURN
      END
      SUBROUTINE EULER(K,L,N,T,DT)
Computes the first order advance. Uses A at T only.
       IMPLICIT REAL*8 (A-H,O-Z)
       COMMON /PLACE/ X(100,3,14),V(100,3,14),A(100,3,14)
        CALL NEWT(X(1,1,K),A(1,1,K),N)
        DO 2 I = 1,N
          DO 2 J = 1,3
            VIJ = V(I,J,K)
            AIJDT = A(I,J,K)*DT
            V(I,J,L) = VIJ + AIJDT
    2       X(I,J,L) = X(I,J,K) + (VIJ + 0.5*AIJDT)*DT
        T = T + DT
       RETURN
      END
      SUBROUTINE LUNGE(K,L,N,T,DT)
Computes a second order advance (Huen's, or 2'nd order Euler).
C  Uses A(t) to probe ahead to X(t + dt) to find A(t + dt)
C  and then averages the two to advance one step.
       IMPLICIT REAL*8 (A-H,O-Z)
       COMMON /PLACE/ X(100,3,14),V(100,3,14),A(100,3,14)
        CALL NEWT (X(1,1,K),A(1,1,K),N)
        CALL PROBE(X(1,1,K),X(1,1,L),V(1,1,K),A(1,1,K),N,DT)
        CALL NEWT (X(1,1,L),A(1,1,L),N)
        DO 2 I = 1,N
          DO 2 J = 1,3
            VIJ = V(I,J,K)
            AIJDT = (A(I,J,K) + A(I,J,L))*0.5*DT
            V(I,J,L) = VIJ + AIJDT
    2       X(I,J,L) = X(I,J,K) + (VIJ + 0.5*AIJDT)*DT
        T = T + DT
       RETURN
      END
      SUBROUTINE RUNGE(K,L,N,T,DT)
Classic Runge-Kutta fourth order advance.
C   1) Use A(t)         to reach x2(t + dt/2) and compute a2(t + dt/2)
C   2) Use a2(t + dt/2) to reach x3(t + dt/2) and compute a3(t + dt/2)
C   3) Use a3(t + dt/2) to reach x4(t + dt)   and compute a4(t + dt/2)
C   4) Use a weighted average of a,a2,a3,a4 to make the actual advance.
       IMPLICIT REAL*8 (A-H,O-Z)
       REAL*8 A3(100,3),A4(100,3)
       COMMON /PLACE/ X(100,3,14),V(100,3,14),A(100,3,14)
        CALL NEWT (X(1,1,K),A(1,1,K),N)
        CALL PROBE(X(1,1,K),X(1,1,L),V(1,1,K),A(1,1,K),N,DT/2)
        CALL NEWT (X(1,1,L),A(1,1,L),N)
        CALL PROBE(X(1,1,K),X(1,1,L),V(1,1,K),A(1,1,L),N,DT/2)
        CALL NEWT (X(1,1,L),A3,N)
        CALL PROBE(X(1,1,K),X(1,1,L),V(1,1,K),A3,N,DT)
        CALL NEWT (X(1,1,L),A4,N)
        DO 2 I = 1,N
          DO 2 J = 1,3
            VIJ = V(I,J,K)
            AIJ = (A(I,J,K) + 2.*(A(I,J,L) + A3(I,J)) + A4(I,J))/6.
            A(I,J,L) = AIJ
            AIJDT = AIJ*DT
            V(I,J,L) = VIJ + AIJDT
    2       X(I,J,L) = X(I,J,K) + (VIJ + 0.5*AIJDT)*DT
        T = T + DT
       RETURN
      END
      SUBROUTINE SCLEAR
       COMMON /STORE/ N,INDEXS(14)
        N = 0
        DO 1 I = 1,14
    1     INDEXS(I) = 14 - I + 1
       RETURN
      END
      SUBROUTINE SGRAB(IT)
       COMMON /STORE/ N,INDEXS(14)
        IT = INDEXS(N)
        N = N - 1
       RETURN
      END
      SUBROUTINE SFREE(IT)
       COMMON /STORE/ N,INDEXS(14)
        IF (IT .LE. 0) RETURN
        N = N + 1
        INDEXS(N) = IT
        IT = 0
       RETURN
      END
      SUBROUTINE MILNE(N,T,T1,DT,EPS,NSTEP)
Chase along according to the Milne predictor-corrector scheme.
C  1) Predict: Fit a parabola to the last three a's. (k-2,k-1,k)
C      Integrate from (k-3) to (k+1) giving v(k+1)
C      Fit a parabola to the latest v's (k-1,k,k+1)
C      Integrate from (k-1) to (k+1) giving x(k+1)
C      Compute a(kp1) at x(k+1), i.e. a(t + dt).
C  2) Correct: Repeat the prediction step, using k-1,k,k+1.
C      There are details. By integrating from (k-3) to (k+1) we have a
C      symmetrical region of the parabola fitted to (k-1),(k),(k+1),
C      which means that P4 is exact for cubics.
C      Secondly, the correction step doesn't need to extrapolate
C      because we have an estimate for (k+1). (The whole point!)
C  3) Check:   The difference between the predicted and corrected a's.
C      Instead of iterating the corrector until (pred - corr) is small,
C      which involves repeated evaluations of the a's at the various
C      corrected x's, all of which are more or less at the same point,
C      the scheme here is to refine the sampling of the whole interval
C      x(k) to x(k+1) by halving the step size, thereby sampling the
C      behaviour at a more even spread of positions.
       IMPLICIT REAL*8 (A-H,O-Z)
       COMMON /STORE/ NAVAIL,INDEXS(14)
       COMMON /PLACE/ X(100,3,14),V(100,3,14),A(100,3,14)
       REAL*8 EXTRAP(100,3)
Compute assorted integrals.
       P4(Y1,Y2,Y3) = 2.*(Y1 + Y3) - Y2
       P3(Y1,Y2,Y3) = Y1 + Y3 + 4.*Y2
       P24(Y1,Y2,Y3) = 2.*Y3 - Y1 + 11.*Y2
        KMAX = 14
        BB = 0
        NSTEP = 0
C
Concoct a past history from which to extrapolate.
        DO 1 K = 1,3
          CALL RUNGE(K,K + 1,N,T,DT)
          CALL JOIN(K,K + 1,N)
    1     CONTINUE
        KP1 = 5
        K   = 4
        KL1 = 3
        KL2 = 2
        KL3 = 1
        KL4 = 0
        KL5 = 0
        KL6 = 0
        NAVAIL = 0
        DO 2 I = 6,KMAX
          NAVAIL = NAVAIL + 1
    2     INDEXS(NAVAIL) = KMAX - I + 6
C
Cook up an estimate of the A's at KP1, one time step ahead.
   10   H3 = DT/3
        H4 = DT*4/3
C       WRITE (6,666) NSTEP,T,DT,  NAVAIL,KL6,KL5,KL4,KL3,KL2,KL1,K,KP1
  666   FORMAT (I4,F7.2,F9.4,14X   ,I3,':',8I3)
        DO 11 I = 1,N
          DO 11 J = 1,3
            VT = V(I,J,KL3) + H4*P4(A(I,J,KL2),A(I,J,KL1),A(I,J,K))
            V(I,J,KP1) = VT
            X(I,J,KP1) = X(I,J,KL1) + H3*P3(V(I,J,KL1),V(I,J,K),VT)
   11       CONTINUE
C
Compute the A's at the extrapolated position, thus involving the DE.
        CALL NEWT(X(1,1,KP1),EXTRAP,N)
C
Correct the X's and V's now that the story at KP1 is known (sortof).
   20   DO 21 I = 1,N
          DO 21 J = 1,3
            VT = V(I,J,KL1) + H3*P3(A(I,J,KL1),A(I,J,K),EXTRAP(I,J))
            V(I,J,KP1) = VT
            X(I,J,KP1) = X(I,J,KL1) + H3*P3(V(I,J,KL1),V(I,J,K),VT)
   21       CONTINUE
C
Calculate new A's to ensure a coherent solution of the DE.
        CALL NEWT(X(1,1,KP1),A(1,1,KP1),N)
C
Compare the provisional and the accepted A's.
   30   B = 0
        DO 32 I = 1,N
          D = 0.
          DP = 0.
          DC = 0.
          DO 31 J = 1,3
            AP = EXTRAP(I,J)
            AC = A(I,J,KP1)
            D = D + (AP - AC)**2
            DP = DP + AP*AP
            DC = DC + AC*AC
   31       CONTINUE
          DPC = DP + DC
          IF (DPC .LE. 0.0) DPC = 1
          D = D/DPC
   32     IF (D .GT. B) B = D
        IF (B .GT. BB) BB = B
C       WRITE (6,667)            B,NAVAIL,KL6,KL5,KL4,KL3,KL2,KL1,K,KP1
  667   FORMAT (20X,         F14.11,I3,':',8I3)
        IF (B .LT. EPS) GO TO 50
C
Chop the step size in half. Interpolate IL1, IL3
   40   CALL SFREE(KL6)
        CALL SFREE(KL5)
        CALL SFREE(KL4)
        CALL SFREE(KL3)
        CALL SGRAB(IL3)
        CALL SGRAB(IL1)
        H24 = DT/24
        DO 41 I = 1,N
          DO 41 J = 1,3
            AK = A(I,J,K)
            AKL1 = A(I,J,KL1)
            AKL2 = A(I,J,KL2)
            VKL1 = V(I,J,KL1)
            VIL1 = VKL1 + H24*P24(AKL2,AKL1,AK)
            VIL3 = VKL1 - H24*P24(AK,AKL1,AKL2)
            V(I,J,IL3) = VIL3
            V(I,J,IL1) = VIL1
            VK = V(I,J,K)
            VKL2 = V(I,J,KL2)
            XKL1 = X(I,J,KL1)
            XIL1 = XKL1 + H24*P24(VKL2,VKL1,VK)
            XIL3 = XKL1 - H24*P24(VK,VKL1,VKL2)
            X(I,J,IL3) = XIL3
   41       X(I,J,IL1) = XIL1
        CALL NEWT(X(1,1,IL1),A(1,1,IL1),N)
        CALL NEWT(X(1,1,IL3),A(1,1,IL3),N)
        KL4 = KL2
        KL3 = IL3
        KL2 = KL1
        KL1 = IL1
        DT = DT/2
        GO TO 10
C
Complete the step by advancing all fingers.
   50   NSTEP = NSTEP + 1
        T = T + DT
        CALL SFREE(KL6)
        KL6 = KL5
        KL5 = KL4
        KL4 = KL3
        KL3 = KL2
        KL2 = KL1
        KL1 = K
        K   = KP1
        CALL SGRAB(KP1)
        CALL JOIN(KL1,K,N)
        IF (T .GE. T1) GO TO 100
C
Consider doubling the step size.
   60   IF (B .GE. EPS/36) GO TO 10
        IF (KL4*KL5*KL6 .LE. 0) GO TO 10
C       WRITE (6,668)              NAVAIL,KL6,KL5,KL4,KL3,KL2,KL1,K,KP1
  668   FORMAT (34X,                I3,':',8I3)
        CALL SFREE(KL1)
        CALL SFREE(KL3)
        CALL SFREE(KL5)
        KL1 = KL2
        KL2 = KL4
        KL3 = KL6
        KL4 = 0
        KL5 = 0
        KL6 = 0
        DT = DT*2
        GO TO 10
Completed.
  100   CALL PLOTXY(0.0,0.0,999)
        WRITE (6,*) "Bmax=",BB
        CALL PRINT(X(1,1,K),V(1,1,K),1,N)
       RETURN
      END
      SUBROUTINE JOIN(K,L,N)
       IMPLICIT REAL*8 (A-H,O-Z)
       REAL U,V
       COMMON /PLACE/ X(100,3,14)
       COMMON /SCOPE/ IU,IV,UMIN,VMIN,US,VS
        DO 1 I = 1,N
          U = (X(I,IU,K) - UMIN)*US
          V = (X(I,IV,K) - VMIN)*VS
          CALL PLOTXY(U,V,3)
          U = (X(I,IU,L) - UMIN)*US
          V = (X(I,IV,L) - VMIN)*VS
C         CALL PLOTXY(U,V,2)
          IT = MOD(I,14)
          CALL SYMBXY(U,V,0.05,IT,0.0,-2)
    1     CONTINUE
       RETURN
      END
      SUBROUTINE PRINT(X,V,IST,LST)
       IMPLICIT REAL*8 (A-H,O-Z)
       REAL*8 X(100,3),V(100,3)
        IF (IST.GT.LST) RETURN
        DO 3 I = IST,LST
          RXYZ = 0.0
          VXYZ = 0.0
          DO 1 J = 1,3
            RXYZ = RXYZ + X(I,J)**2
   1        VXYZ = VXYZ + V(I,J)**2
            RXYZ = DSQRT(RXYZ)
            VXYZ = DSQRT(VXYZ)
          WRITE (6,2) I,(X(I,J),J = 1,3),RXYZ,(V(I,J),J = 1,3),VXYZ
   2      FORMAT (I3,2(4F8.5,3X))
   3      CONTINUE
       RETURN
      END
      FUNCTION PE(N,L)
       IMPLICIT REAL*8 (A-H,O-Z)
       REAL*8 M(100)
       COMMON /CONST/ G,CLOSE,M
       COMMON /PLACE/ X(100,3,14)
        T = 0
        NL1 = 1
        DO 2 I = 1,NL1
          IP1 = I + 1
          DO 2 J = IP1,N
            R2 = 0.0
            DO 1 K = 1,3
    1         R2 = R2 + (X(I,K,L) - X(J,K,L))**2
    2      T = T - M(I)*M(J)/DSQRT(R2)
        PE = G*T
       RETURN
      END
      REAL FUNCTION KE(N,L)
       IMPLICIT REAL*8 (A-H,O-Z)
       REAL*8 M(100)
       COMMON /CONST/ G,CLOSE,M
       COMMON /PLACE/ X(100,3,14),V(100,3,14)
        T = 0
        DO 2 I = 1,N
          V2 = 0
          DO 1 J = 1,3
    1       V2 = V2 + V(I,J,L)**2
    2     T = T + M(I)*V2
        KE = T/2
       RETURN
      END
      SUBROUTINE COM(N,L,TM,W,P)
Centre of mass.....
       IMPLICIT REAL*8 (A-H,O-Z)
       REAL*8 M(100),W(3),P(3)
       COMMON /PLACE/ X(100,3,14),V(100,3,14)
       COMMON /CONST/ G,CLOSE,M
        DO 1 I = 1,3
          W(I) = 0
    1     P(I) = 0
        TM = 0
        DO 2 I = 1,N
          TM = TM + M(I)
          DO 2 J = 1,3
            W(J) = W(J) + M(I)*X(I,J,L)
    2       P(J) = P(J) + M(I)*V(I,J,L)
        DO 3 J = 1,3
          W(J) = W(J)/TM
    3     P(J) = P(J)/TM
       RETURN
      END
      IMPLICIT REAL*8 (A-H,O-Z)
      LOGICAL PRANGE,ROTATE,DIVE
      LOGICAL ASIS
      INTEGER FANCY
      INTEGER DEVICE(10),CARDS,BLANK
      REAL*8 M(100)
      REAL*8 XMIN(3),XMAX(3),W(3),P(3)
      REAL XSIZE,YSIZE,B,BLOAT
      COMMON /PLOTIT/ ROTATE,DIVE,DEVICE,BLOAT
      COMMON /CONST/ G,CLOSE,M
      COMMON /PLACE/ X(100,3,14),V(100,3,14),A(100,3,14)
      COMMON /SCOPE/ IU,IV,UMIN,VMIN,US,VS
      COMMON LINPR,CARDS
      DATA BLANK/'    '/
      DIVE = .FALSE.
      DIVE = .TRUE.
      ASIS = .FALSE.
      CARDS = 5
      LINPR = 6
      IN = 10
      XSIZE = 9
      YSIZE = 4.75
      DEVICE(1) = BLANK
      BLOAT = 10/(0.5 + 7.1)
      WRITE (LINPR,1)
    1 FORMAT (' Star Trails.')
      IF (.NOT.DIVE) WRITE (LINPR,2)
    2 FORMAT (' Enter size, (4.75 or 10.5)')
      IF (.NOT.DIVE) READ (CARDS,*) XSIZE
      YSIZE = XSIZE
      IN = 10
      OPEN (IN,FILE="TCL.dat")
      READ (IN,*) N
      WRITE (LINPR,*) N," bodies."
      READ (IN,*) G
      WRITE (LINPR,*) G," gravitational constant."
      READ (IN,*) T1,DT
      WRITE (LINPR,*) T1,DT," Run time, time step."
      READ (IN,*) XMIN,XMAX
      DO 3 I = 1,N
        READ (IN,*) M(I),(X(I,J,1), J = 1,3),(V(I,J,1),J = 1,3)
        DO 3 J = 1,3
          A(I,J,1) = 0
    3   CONTINUE
      CLOSE (IN)
c      WRITE (LINPR,4) N
      IST = 1
      LST = 3
    4 FORMAT (' The first and last body to show (of',I3,')')
c      READ (CARDS,*) IST,LST
      FANCY = 1
      WRITE (LINPR,5)
    5 FORMAT (' Euler/2''nd/Runge/Milne (1/2/3/4)')
      READ (CARDS,*) FANCY
c      WRITE (LINPR,6)
c    6 FORMAT (' Centre of mass (T/F)')
c      READ (CARDS,*) ASIS
      ASIS = .NOT.ASIS
C
   10 DO 11 I = 1,3
        IF (XMIN(I) .GE. XMAX(I)) GO TO 12
   11   CONTINUE
      GO TO 15
   12 DO 13 I = 1,3
        XMIN(I) = X(1,I,1)
   13   XMAX(I) = X(1,I,1)
      DO 14 I = 1,N
         DO 14 J = 1,3
           XMIN(J) = DMIN1(XMIN(J),X(I,J,1))
   14      XMAX(J) = DMAX1(XMAX(J),X(I,J,1))
   15 CLOSE = 0
      DO 16 I = 1,3
   16   CLOSE = CLOSE + (XMAX(I) - XMIN(I))**2
      IF (ASIS) GO TO 20
      CALL COM(N,1,TM,W,P)
      DO 17 I = 1,N
        DO 17 J = 1,3
   17     V(I,J,1) = V(I,J,1) - P(J)
C
   20 IU = 1
      IV = 2
      UMIN = XMIN(IU)
      VMIN = XMIN(IV)
      B = 0.5
      US = (XSIZE - 0)/(XMAX(IU) - UMIN)
      VS = (YSIZE - 0)/(XMAX(IV) - VMIN)
      IF (.NOT.PRANGE(-B,XSIZE + B,-B,YSIZE + B)) STOP
      CALL DOBOX(0.0,0.0,XSIZE,YSIZE)
C
  100 T0 = 0
      T = T0
      NSTEP = T1/DT + 0.5
      IF (IST .LE. LST) WRITE (LINPR,101)
  101 FORMAT (8X,'x       y       z       R',
     1 9X,'vx      vy      vz       V')
      IF (IST .LE. LST) CALL PRINT(X,V,IST,LST)
      IF (FANCY .EQ. 4) GO TO 200
      I1 = 1
      I2 = 2
      DO 110 I = 1,NSTEP
        IF (FANCY .EQ. 1) CALL EULER(I1,I2,N,T,DT)
        IF (FANCY .EQ. 2) CALL LUNGE(I1,I2,N,T,DT)
        IF (FANCY .EQ. 3) CALL RUNGE(I1,I2,N,T,DT)
        WRITE (LINPR,*) "T=",T
        CALL JOIN(I1,I2,N)
        CALL SWAPI(I1,I2)
        IF (IST .LE. LST) CALL PRINT(X(1,1,I1),V(1,1,I1),1,N)
  110   CONTINUE
      GO TO 9000
C
  200 EPS = 1
  201 EPS = EPS/2
      IF (1 + EPS .NE. 1) GO TO 201
      EPS = EPS*2
      CALL MILNE(N,T,T1,DT,EPS,NSTEP)
C
 9000 IF (FANCY .NE. 4) CALL PLOTXY(0.0,0.0,999)
      WRITE (LINPR,*) "Reached T=",T
      WRITE (LINPR,*) "Target  T=",T1
      WRITE (LINPR,*) "Time step=",DT,"Nstep=",NSTEP
      IF (FANCY .NE. 4) CALL PRINT(X(1,1,I1),V(1,1,I1),1,N)
      CLOSE = DSQRT(CLOSE)
      WRITE (LINPR,9001) CLOSE
 9001 FORMAT (' Closest approach:',E15.6)
      END

```



### Results


### =TCL matching?=

I have been unable to follow the exact workings of the TCL example in the absence of annotations, in particular whether I am looking at x1 x2 x3 y1 y2 y3 z1 z2 z3 or, x1 y1 z1, etc. My file TCL.dat is as follows:

```txt

3                    Bodies
0.01                 Gravitational constant.
0.2, 0.01            Run time, time step.
-2 -2 -2, +2 +2 +2   xyzmin, xyzmax for plot scaling
1,      0 0 0,  0.01  0     0          Mass, xyz Position, xyz Velocity
0.1,    1 1 0,  0     0     0.02
0.001   0 1 1,  0.01 -0.01 -0.01

```

This relies on Fortran's free-format reading looking only for the requested count of numbers on a line, so that what follows is ignored and so can be annotation. More generally, with this style of input a / character indicates an in-line comment and is treated as end-of-line for the reading of values. Output:

```txt

 Star Trails.
           3  bodies.
  1.000000000000000E-002  gravitational constant.
  0.200000000000000       1.000000000000000E-002  Run time, time step.
 Euler/2'nd/Runge/Milne (1/2/3/4)
1
        x       y       z       R         vx      vy      vz       V
  1 0.00000 0.00000 0.00000 0.00000    0.01000 0.00000 0.00000 0.01000
  2 1.00000 1.00000 0.00000 1.41421    0.00000 0.00000 0.02000 0.02000
  3 0.00000 1.00000 1.00000 1.41421    0.01000-0.01000-0.01000 0.01732
 T=  1.000000000000000E-002
  1 0.00010 0.00000 0.00000 0.00010    0.01000 0.00000 0.00000 0.01000
  2 1.00000 1.00000 0.00020 1.41421   -0.00004-0.00004 0.02000 0.02000
  3 0.00010 0.99990 0.99990 1.41407    0.01000-0.01004-0.01004 0.01737
 T=  2.000000000000000E-002
  1 0.00020 0.00000 0.00000 0.00020    0.01001 0.00001 0.00000 0.01001
  2 1.00000 1.00000 0.00040 1.41421   -0.00007-0.00007 0.02000 0.02000
  3 0.00020 0.99980 0.99980 1.41393    0.01001-0.01007-0.01008 0.01741
 T=  3.000000000000000E-002
  1 0.00030 0.00000 0.00000 0.00030    0.01001 0.00001 0.00000 0.01001
  2 1.00000 1.00000 0.00060 1.41421   -0.00011-0.00011 0.02000 0.02000
  3 0.00030 0.99970 0.99970 1.41379    0.01001-0.01011-0.01012 0.01746
 T=  4.000000000000000E-002
  1 0.00040 0.00000 0.00000 0.00040    0.01001 0.00001 0.00000 0.01001
  2 1.00000 1.00000 0.00080 1.41421   -0.00014-0.00014 0.02000 0.02000
  3 0.00040 0.99960 0.99960 1.41364    0.01001-0.01014-0.01016 0.01750
 T=  5.000000000000000E-002
  1 0.00050 0.00000 0.00000 0.00050    0.01002 0.00002 0.00000 0.01002
  2 1.00000 1.00000 0.00100 1.41421   -0.00018-0.00018 0.02000 0.02000
  3 0.00050 0.99950 0.99950 1.41350    0.01002-0.01018-0.01019 0.01755
 T=  6.000000000000000E-002
  1 0.00060 0.00000 0.00000 0.00060    0.01002 0.00002 0.00000 0.01002
  2 0.99999 0.99999 0.00120 1.41421   -0.00021-0.00021 0.02000 0.02000
  3 0.00060 0.99939 0.99939 1.41336    0.01002-0.01021-0.01023 0.01759
 T=  7.000000000000001E-002
  1 0.00070 0.00000 0.00000 0.00070    0.01002 0.00003 0.00000 0.01002
  2 0.99999 0.99999 0.00140 1.41420   -0.00025-0.00025 0.02000 0.02000
  3 0.00070 0.99929 0.99929 1.41321    0.01002-0.01025-0.01027 0.01764
 T=  8.000000000000000E-002
  1 0.00080 0.00000 0.00000 0.00080    0.01003 0.00003 0.00000 0.01003
  2 0.99999 0.99999 0.00160 1.41420   -0.00028-0.00028 0.02000 0.02000
  3 0.00080 0.99919 0.99919 1.41307    0.01003-0.01028-0.01031 0.01768
 T=  9.000000000000000E-002
  1 0.00090 0.00000 0.00000 0.00090    0.01003 0.00003 0.00000 0.01003
  2 0.99999 0.99999 0.00180 1.41419   -0.00032-0.00032 0.02000 0.02001
  3 0.00090 0.99909 0.99908 1.41292    0.01003-0.01032-0.01035 0.01773
 T=  0.100000000000000
  1 0.00100 0.00000 0.00000 0.00100    0.01004 0.00004 0.00000 0.01004
  2 0.99998 0.99998 0.00200 1.41419   -0.00035-0.00035 0.02000 0.02001
  3 0.00100 0.99898 0.99898 1.41277    0.01004-0.01035-0.01039 0.01777
 T=  0.110000000000000
  1 0.00110 0.00000 0.00000 0.00110    0.01004 0.00004 0.00000 0.01004
  2 0.99998 0.99998 0.00220 1.41418   -0.00039-0.00039 0.02000 0.02001
  3 0.00110 0.99888 0.99888 1.41263    0.01004-0.01039-0.01043 0.01782
 T=  0.120000000000000
  1 0.00120 0.00000 0.00000 0.00120    0.01004 0.00004 0.00000 0.01004
  2 0.99997 0.99997 0.00240 1.41418   -0.00042-0.00042 0.02000 0.02001
  3 0.00120 0.99877 0.99877 1.41248    0.01004-0.01042-0.01047 0.01786
 T=  0.130000000000000
  1 0.00130 0.00000 0.00000 0.00130    0.01005 0.00005 0.00000 0.01005
  2 0.99997 0.99997 0.00260 1.41417   -0.00046-0.00046 0.02000 0.02001
  3 0.00130 0.99867 0.99867 1.41233    0.01005-0.01046-0.01051 0.01791
 T=  0.140000000000000
  1 0.00140 0.00000 0.00000 0.00140    0.01005 0.00005 0.00000 0.01005
  2 0.99997 0.99997 0.00280 1.41417   -0.00050-0.00050 0.02000 0.02001
  3 0.00140 0.99857 0.99856 1.41218    0.01005-0.01050-0.01055 0.01795
 T=  0.150000000000000
  1 0.00150 0.00000 0.00000 0.00150    0.01005 0.00005 0.00000 0.01005
  2 0.99996 0.99996 0.00300 1.41416   -0.00053-0.00053 0.02000 0.02001
  3 0.00150 0.99846 0.99846 1.41203    0.01005-0.01053-0.01058 0.01800
 T=  0.160000000000000
  1 0.00160 0.00000 0.00000 0.00160    0.01006 0.00006 0.00000 0.01006
  2 0.99995 0.99995 0.00320 1.41415   -0.00057-0.00057 0.02000 0.02002
  3 0.00160 0.99835 0.99835 1.41188    0.01006-0.01057-0.01062 0.01805
 T=  0.170000000000000
  1 0.00171 0.00001 0.00000 0.00171    0.01006 0.00006 0.00000 0.01006
  2 0.99995 0.99995 0.00340 1.41415   -0.00060-0.00060 0.02000 0.02002
  3 0.00171 0.99825 0.99824 1.41173    0.01006-0.01060-0.01066 0.01809
 T=  0.180000000000000
  1 0.00181 0.00001 0.00000 0.00181    0.01006 0.00006 0.00000 0.01006
  2 0.99994 0.99994 0.00360 1.41414   -0.00064-0.00064 0.02000 0.02002
  3 0.00181 0.99814 0.99814 1.41158    0.01006-0.01064-0.01070 0.01814
 T=  0.190000000000000
  1 0.00191 0.00001 0.00000 0.00191    0.01007 0.00007 0.00000 0.01007
  2 0.99994 0.99994 0.00380 1.41413   -0.00067-0.00067 0.02000 0.02002
  3 0.00191 0.99804 0.99803 1.41143    0.01007-0.01067-0.01074 0.01818
 T=  0.200000000000000
  1 0.00201 0.00001 0.00000 0.00201    0.01007 0.00007 0.00000 0.01007
  2 0.99993 0.99993 0.00400 1.41412   -0.00071-0.00071 0.02000 0.02002
  3 0.00201 0.99793 0.99792 1.41128    0.01007-0.01071-0.01078 0.01823
 Reached T=  0.200000000000000
 Target  T=  0.200000000000000
 Time step=  1.000000000000000E-002 Nstep=          20
  1 0.00201 0.00001 0.00000 0.00201    0.01007 0.00007 0.00000 0.01007
  2 0.99993 0.99993 0.00400 1.41412   -0.00071-0.00071 0.02000 0.02002
  3 0.00201 0.99793 0.99792 1.41128    0.01007-0.01071-0.01078 0.01823
 Closest approach:   0.140874E+01

```



### =Preparing parameters=

Another possible run is for the earth's orbit. For a circular orbit, taking the unit of distance being its radius, the unit of mass being the sun, and the unit of time being a year, G comes out as <math>4 \pi^2</math> though ''not'' as a pure number, its has the usual dimensions. Finding a coherent set of values for the Sun-Earth-Moon system was frustrating as the internet is littered with different values. Referring to the CRC handbook (87<sup>'th</sup> edition, 2006-7), G = 6·6742(10)±0·0031 E-11, Ms = 1·98844E30 Kg, Me = 5·9742E24 Kg, Mm = 7·3483E22 Kg, Re = 1AU = 1·49597870E11 metres (actually the semi-major axis), Rm = 3·844E8 metres. However, the handbook also contains varying values, such as for the mass of the sun. Notoriously, the value of G is both difficult to measure and varying in result. My copy of Resnick & Halliday (1966) has 6·670±0·015 for example.

The idea was to start with an initial state having the Sun, Earth and Moon all in a straight line along the x-axis, but after some ad-hoc messing about, confusions escalated to the degree that an ad-hoc calculation prog. was in order. This turns out to need a cube root function, and a proper solution for this raises again the utility of palindromic functions. Later Fortran supplies intrinsic functions such as EXPONENT(x) which returns the exponent part of the floating-point number, ''x'' and extracting this would be useful in devising an initial value for an iterative refinement calculation. Clearly, one wants power/3 for this, and so being able to write something like <code>EXPONENT(t) = EXPONENT(x)/3</code> would help, along with similar usage of the FRACTION(x) function - omitting details such as the remainder when the power is divided by three. The same ideas arise with the square root function, though here, SQRT is already supplied. Alas, only the SUBSTR intrinsic function of pl/i has palindromic usage.

```Fortran

Calculate some parameters for the solar system of Sun, Earth and Moon.
      IMPLICIT REAL*8 (A-Z)	!No integers need apply.
      CBRT(X) = SIGN(EXP(LOG(ABS(X))/3),X)	!Crude. Fails for zero.
      PI = 4*ATAN(1D0)
      G = 6.674210D-11		!Gravitational constant, MKS units.
      MS = 1.98844D+30		!Mass of the sun.
      ME = 5.9742D+24		!Mass of the earth.
      MM = 7.3483D+22		!Mass of the moon.
      RE = 1.49597870D+11	!Radius of the earth's orbit around the sun. 1 Astronomical Unit = semi-major axis.
      RM = 3.844D+8		!Radius of the moon's orbit around the earth.
      YS = 31556925.9747D0	!Earth's tropical year, in seconds. This includes precession.
      Y = YS/24/3600            !In days. This is *not* the proper orbit-repetition time!

      WRITE (6,*) Y,"Tropical year, days."
      WRITE (6,*) "Earth's orbit taken as circular, but using Rmax."
      WRITE (6,*) 2*PI*SQRT(RE**3/(G*MS))/3600/24,"Year, in days"
      WRITE (6,*) 2*PI*SQRT(RE**3/(G*(MS + ME)))/3600/24,"Me included."
      RC = CBRT(G*(MS + ME)*YS**2/(4*PI**2))
      WRITE (6,*) "Circular orbit of period one tropical year."
      WRITE (6,*) RC,"Rc: Earth's circular orbit radius."
      WRITE (6,*) RE,"Re: Actual semi-major axis."
      WRITE (6,*) RE - RC,"Re - Rc"
      OS  = RC*ME/MS
      WRITE (6,*) OS ,"Os: Sun's offset from CoM due to Earth at Rc."
      VE = 2*PI*RC/YS
      WRITE (6,*) VE,"Ve: Earth's circular orbit velocity."
      WS = 2*PI*OS/YS
      WRITE (6,*) WS,"Ws: Sun's circular orbit velocity due to Earth."

      WRITE (6,*)
      WRITE (6,*) RM,"Rm: radius of the moon's orbit around the earth."
      TM = 2*PI*SQRT(RM**3/(G*(ME + MM)))/3600/24
      WRITE (6,*) TM,"Tm: time for the moon's circular orbit, days."
      OE = RM*MM/ME
      WRITE (6,*) OE,"Oe: Earth's offset from CoM due to Moon at Rm."
      VM = 2*PI*RM/(TM*3600*24)
      WRITE (6,*) VM,"Vm: Moon's circular orbit velocity."
      WE = 2*PI*OE/(TM*3600*24)
      WRITE (6,*) WE,"We: Earth's circular orbit velocity due to Moon."
Combine the offsets and wobbles for the Sun-----Earth-Moon in a straight line
      WRITE (6,*)
      WRITE (6,*) "   (CoM)"
      WRITE (6,*) "Sun--0-----------------------------Earth--Moon-->x"
      RC = CBRT(G*(MS + ME + MM)*YS**2/(4*PI**2))
      WRITE (6,*) RC," Rc: Earth's circular orbit for Sun+Earth+Moon."
      SX = -(ME*(RC - OE) + MM*(RC + RM))/MS
      WRITE (6,*) SX," Sx: Sun's   x-position, offset by Earth+Moon."
      SVY = 2*PI*SX/YS
      WRITE (6,*) SVY,"SVy: Sun's   y-velocity."
      EX = RC - OE
      WRITE (6,*) EX," Ex: Earth's x-position, offset by Moon."
      EVY = VE - WE
      WRITE (6,*) EVY,"EVy: Earth's y-velocity."
      MX = RC + RM
      WRITE (6,*) MX," Mx: Moon's  x-position."
      MVY = VE + VM
      WRITE (6,*) MVY,"MVy: Moon's  y-velocity."
Convert to 'AU', being one earth orbit radius for a circular orbit taking one year.
      WRITE (6,10)
   10 FORMAT (/," Time in years, masses in suns, distances in 'AU'",
     1 /,12X,"Solar masses        x-position        y-velocity.")
      WRITE (6,11) "Sun",1.0,SX/RC,SVY/RC*YS
      WRITE (6,11) "Earth",ME/MS,EX/RC,EVY/RC*YS
      WRITE (6,11) "Moon",MM/MS,MX/RC,MVY/RC*YS
   11 FORMAT (A6,3F18.14)
      WRITE (6,*)
      WRITE (6,*) 4*PI**2,"4Pi^2: G in AU/year units."
      WRITE (6,*) 2*PI,"2Pi: distance per year for R = 1."
      END

```

Notably, wanting a circular orbit to ease inspection of the results meant that the actual semi-major axis of the earth's elliptical orbit could not be used as in the first trials. Instead, the orbital period was taken as a year (wrongly so, as this includes the precession of the earth's axis of revolution, with a period of  about 26,000 years) and the radius of a circular orbit having that period determined. This however is affected by whether the calculation is for the sun alone (so, a massless Earth), or, the sun and the earth together, or the sun, earth and moon together. As well, there is no z-action: the moon's orbital plane is deemed the same as that of the earth and it isn't. Results:

```txt

  365.242198781250      Tropical year, days.
Earth's orbit taken as circular, but using Rmax.
  365.256591252027      Year, in days
  365.256042552793      Me included.
Circular orbit of period one tropical year.
  149594089981.644      Rc: Earth's circular orbit radius.
  149597870000.000      Re: Actual semi-major axis.
  3780018.35565186      Re - Rc
  449450.329086289      Os: Sun's offset from CoM due to Earth at Rc.
  29785.1377845590      Ve: Earth's circular orbit velocity.
 8.948842819120138E-002 Ws: Sun's circular orbit velocity due to Earth.

  384400000.000000      Rm: radius of the moon's orbit around the earth.
  27.2801498336002      Tm: time for the moon's circular orbit, days.
  4728141.87673663      Oe: Earth's offset from CoM due to Moon at Rm.
  1024.71419780640      Vm: Moon's circular orbit velocity.
  12.6040429509235      We: Earth's circular orbit velocity due to Moon.

   (CoM)
Sun--0-----------------------------Earth--Moon-->x
  149594091824.393       Rc: Earth's circular orbit for Sun+Earth+Moon.
 -454978.599317466       Sx: Sun's   x-position, offset by Earth+Moon.
-9.058914206677031E-002 SVy: Sun's   y-velocity.
  149589363682.517       Ex: Earth's x-position, offset by Moon.
  29772.5337416081      EVy: Earth's y-velocity.
  149978491824.393       Mx: Moon's  x-position.
  30809.8519823654      MVy: Moon's  y-velocity.

Time in years, masses in suns, distances in 'AU'
           Solar masses        x-position        y-velocity.
  Sun  1.00000000000000 -0.00000304142091 -0.00001910981119
Earth  0.00000300446581  0.99996839352531  6.28052640251386
 Moon  0.00000003695510  1.00256962019898  6.49934904809141

  39.4784176043574      4Pi^2: G in AU/year units.
  6.28318530717959      2Pi: distance per year for R = 1.

```


====Sun-Earth-Moon====
This leads to file SEM.dat, as follows:

```txt

3                    Bodies
39.478417604357434   G = 4Pi**2: Solar mass = 1, Earth's circular equivalent-time orbit radius = 1, time unit = 1 year.
1.0, 0.001           Run time, time step. (years)
-2 -2 -2, +2 +2 +2   xyzmin, xyzmax for plot scaling
1,               -0.00000304142091 0 0,   0 -0.00001910981119 0      Solar Mass,  xyz Position, xyz Velocity
3.00446581E-6,    0.99996839352531 0 0,   0  6.28052640251386 0      Earth completes one circle of radius 1 in 1 year, so V = 2Pi AU/year.
3.69551E-8,       1.00256962019898 0 0,   0  6.49934904809141 0      Sun-Earth-Moon in a straight line along the x-axis.
```

The initial results...

```txt

 Star Trails.
           3  bodies.
   39.4784176043574       gravitational constant.
   1.00000000000000       1.000000000000000E-003  Run time, time step.
 Euler/2'nd/Runge/Milne (1/2/3/4)
3
        x       y       z       R         vx      vy      vz       V
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99997 0.00000 0.00000 0.99997    0.00000 6.28053 0.00000 6.28053
  3 1.00257 0.00000 0.00000 1.00257    0.00000 6.49935 0.00000 6.49935
 T=  1.000000000000000E-003
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99995 0.00628 0.00000 0.99997   -0.03927 6.28041 0.00000 6.28053
  3 1.00254 0.00650 0.00000 1.00256   -0.05678 6.49849 0.00000 6.49873
 T=  2.000000000000000E-003
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99989 0.01256 0.00000 0.99997   -0.07853 6.28007 0.00000 6.28056
  3 1.00246 0.01300 0.00000 1.00254   -0.11343 6.49590 0.00000 6.49689
 T=  3.000000000000000E-003
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99979 0.01884 0.00000 0.99997   -0.11780 6.27949 0.00000 6.28060
  3 1.00231 0.01949 0.00000 1.00250   -0.16981 6.49162 0.00000 6.49384
 T=  4.000000000000000E-003
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99965 0.02512 0.00000 0.99997   -0.15706 6.27869 0.00000 6.28065
  3 1.00212 0.02598 0.00000 1.00245   -0.22579 6.48567 0.00000 6.48960
 T=  5.000000000000000E-003
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99948 0.03140 0.00000 0.99997   -0.19633 6.27765 0.00000 6.28072
  3 1.00186 0.03246 0.00000 1.00239   -0.28125 6.47811 0.00000 6.48422
 T=  6.000000000000000E-003
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99926 0.03767 0.00000 0.99997   -0.23559 6.27638 0.00000 6.28080
  3 1.00155 0.03893 0.00000 1.00231   -0.33606 6.46900 0.00000 6.47772
 T=  7.000000000000000E-003
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99901 0.04395 0.00000 0.99997   -0.27486 6.27488 0.00000 6.28090
  3 1.00119 0.04540 0.00000 1.00222   -0.39013 6.45840 0.00000 6.47017
 T=  8.000000000000000E-003
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99871 0.05022 0.00000 0.99997   -0.31412 6.27314 0.00000 6.28100
  3 1.00077 0.05185 0.00000 1.00212   -0.44335 6.44639 0.00000 6.46162
 T=  9.000000000000001E-003
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99838 0.05650 0.00000 0.99998   -0.35339 6.27117 0.00000 6.28112
  3 1.00031 0.05829 0.00000 1.00200   -0.49563 6.43307 0.00000 6.45214
 T=  1.000000000000000E-002
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99801 0.06277 0.00000 0.99998   -0.39265 6.26897 0.00000 6.28125
  3 0.99978 0.06472 0.00000 1.00188   -0.54689 6.41853 0.00000 6.44179

etc...

 T=  0.998000000000001     
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99995-0.01262 0.00000 1.00003    0.07901 6.28523 0.00000 6.28573
  3 0.99728-0.01296 0.00000 0.99736    0.10301 6.07537 0.00000 6.07625
 T=  0.999000000000001     
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 1.00001-0.00633 0.00000 1.00003    0.03933 6.28558 0.00000 6.28570
  3 0.99737-0.00688 0.00000 0.99739    0.07940 6.07848 0.00000 6.07899
 T=   1.00000000000000     
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 1.00003-0.00005 0.00000 1.00003   -0.00034 6.28565 0.00000 6.28565
  3 0.99744-0.00080 0.00000 0.99744    0.05550 6.08257 0.00000 6.08283
 Reached T=   1.00000000000000     
 Target  T=   1.00000000000000     
 Time step=  1.000000000000000E-003 Nstep=        1000
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 1.00003-0.00005 0.00000 1.00003   -0.00034 6.28565 0.00000 6.28565
  3 0.99744-0.00080 0.00000 0.99744    0.05550 6.08257 0.00000 6.08283
 Closest approach:   0.260123E-02

```

Using instead the first-order method, a year into the calculation the results are not so good:

```txt

 T=  0.998000000000001     
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 1.02057-0.19728 0.00000 1.03947    1.16628 6.04742 0.00000 6.15886
  3 1.01296-0.09530 0.00000 1.01743    0.58893 6.20845 0.00000 6.23632
 T=  0.999000000000001     
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 1.02172-0.19123 0.00000 1.03947    1.13041 6.05436 0.00000 6.15898
  3 1.01353-0.08909 0.00000 1.01744    0.55096 6.21201 0.00000 6.23640
 T=   1.00000000000000     
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 1.02284-0.18518 0.00000 1.03946    1.09449 6.06108 0.00000 6.15911
  3 1.01406-0.08288 0.00000 1.01744    0.51297 6.21534 0.00000 6.23647
 Reached T=   1.00000000000000     
 Target  T=   1.00000000000000     
 Time step=  1.000000000000000E-003 Nstep=        1000
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 1.02284-0.18518 0.00000 1.03946    1.09449 6.06108 0.00000 6.15911
  3 1.01406-0.08288 0.00000 1.01744    0.51297 6.21534 0.00000 6.23647
 Closest approach:   0.163788E-02

```

The earth and moon go wandering. With too large a step size, the situation between steps can be very different from those at the start and stop of a step. The predictor-corrector scheme does better:

```txt

 Star Trails.
           3  bodies.
   39.4784176043574       gravitational constant.
   1.00000000000000       1.000000000000000E-003  Run time, time step.
 Euler/2'nd/Runge/Milne (1/2/3/4)
        x       y       z       R         vx      vy      vz       V
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 0.99997 0.00000 0.00000 0.99997    0.00000 6.28053 0.00000 6.28053
  3 1.00257 0.00000 0.00000 1.00257    0.00000 6.49935 0.00000 6.49935
 Bmax=  8.092753583528147E-010
  1 0.00000 0.00000 0.00000 0.00000    0.00000-0.00002 0.00000 0.00002
  2 1.00003-0.00007 0.00000 1.00003    0.00061 6.28576 0.00000 6.28576
  3 0.99733 0.00007 0.00000 0.99733   -0.01260 6.07555 0.00000 6.07556
 DTmin=  3.906250000000000E-006 , DTmax=  2.000000000000000E-003
 Reached T=   1.00001171874997     
 Target  T=   1.00000000000000     
 Time step=  5.000000000000000E-004 Nstep=        1755
 Closest approach:   0.260123E-02

```

Notice that it tries an increased time step but also a much smaller time step, in the end requiring 1,755 steps. Part of the project was to assess various schemes for changing the step size and on what basis. This also means that the calculation may well not produce results at desired times, not just because repeated addition of a floating-point number such as 0.001 may not produce a value such as 5.755 exactly, but also because the varying step size might hop past it.

More generally, when two bodies approach closely the resulting curvature forces a smaller step size, which is wasted when calculating details for widely-separated bodies. One ploy is to treat such pairs via two-body formulae (in their centre-of-mass coordinates) for the time of their closeness, retaining a larger time step for the rest of the calculation. This can be further generalised into clumping nearby bodies into a single mass when considering their effect on far-distant bodies. In all of this, the administration requirements become ever-more complex.


## Go

{{trans|C}}

```go
package main

import (
    "fmt"
    "math"
    "os"
)

type vector struct{ x, y, z float64 }

func (v vector) add(w vector) vector {
    return vector{v.x + w.x, v.y + w.y, v.z + w.z}
}

func (v vector) sub(w vector) vector {
    return vector{v.x - w.x, v.y - w.y, v.z - w.z}
}

func (v vector) scale(m float64) vector {
    return vector{v.x * m, v.y * m, v.z * m}
}

func (v vector) mod() float64 {
    return math.Sqrt(v.x*v.x + v.y*v.y + v.z*v.z)
}

var (
    bodies, timeSteps                    int
    masses                               []float64
    gc                                   float64
    positions, velocities, accelerations []vector
)

func initiateSystem(fileName string) error {
    file, err := os.Open(fileName)
    if err != nil {
        return err
    }
    defer file.Close()
    fmt.Fscanf(file, "%f%d%d", &gc, &bodies, &timeSteps)
    masses = make([]float64, bodies)
    positions = make([]vector, bodies)
    velocities = make([]vector, bodies)
    accelerations = make([]vector, bodies)
    for i := 0; i < bodies; i++ {
        fmt.Fscanf(file, "%f", &masses[i])
        fmt.Fscanf(file, "%f%f%f", &positions[i].x, &positions[i].y, &positions[i].z)
        fmt.Fscanf(file, "%f%f%f", &velocities[i].x, &velocities[i].y, &velocities[i].z)
    }
    return nil
}

func resolveCollisions() {
    for i := 0; i < bodies-1; i++ {
        for j := i + 1; j < bodies; j++ {
            if positions[i] == positions[j] {
                velocities[i], velocities[j] = velocities[j], velocities[i]
            }
        }
    }
}

func computeAccelerations() {
    for i := 0; i < bodies; i++ {
        accelerations[i] = vector{0, 0, 0}
        for j := 0; j < bodies; j++ {
            if i != j {
                temp := gc * masses[j] / math.Pow(positions[i].sub(positions[j]).mod(), 3)
                accelerations[i] = accelerations[i].add(positions[j].sub(positions[i]).scale(temp))
            }
        }
    }
}

func computeVelocities() {
    for i := 0; i < bodies; i++ {
        velocities[i] = velocities[i].add(accelerations[i])
    }
}

func computePositions() {
    for i := 0; i < bodies; i++ {
        positions[i] = positions[i].add(velocities[i].add(accelerations[i].scale(0.5)))
    }
}

func simulate() {
    computeAccelerations()
    computePositions()
    computeVelocities()
    resolveCollisions()
}

func printResults() {
    f := "Body %d : % 8.6f  % 8.6f  % 8.6f | % 8.6f  % 8.6f  % 8.6f\n"
    for i := 0; i < bodies; i++ {
        fmt.Printf(
            f, i+1,
            positions[i].x, positions[i].y, positions[i].z,
            velocities[i].x, velocities[i].y, velocities[i].z,
        )
    }
}

func main() {
    if len(os.Args) != 2 {
        fmt.Printf("Usage : %s <file name containing system configuration data>\n", os.Args[0])
    } else {
        err := initiateSystem(os.Args[1])
        if err != nil {
            fmt.Println(err)
            return
        }
        fmt.Print("Body   :      x          y          z    |")
        fmt.Println("     vx         vy         vz")
        for i := 0; i < timeSteps; i++ {
            fmt.Printf("\nCycle %d\n", i+1)
            simulate()
            printResults()
        }
    }
}
```


Contents of nbody.txt:

```txt

0.01 3 20
1
0 0 0
0.01 0 0
0.1
1 1 0
0 0 0.02
0.001
0 1 1
0.01 -0.01 -0.01

```


{{out}}

```txt

Body   :      x          y          z    |     vx         vy         vz

Cycle 1
Body 1 :  0.010177   0.000179   0.000002 |  0.010354   0.000357   0.000004
Body 2 :  0.998230   0.998232   0.020002 | -0.003539  -0.003536   0.020004
Body 3 :  0.010177   0.988232   0.988055 |  0.010354  -0.013536  -0.013889

Cycle 2
Body 1 :  0.020709   0.000718   0.000011 |  0.010710   0.000721   0.000014
Body 2 :  0.992907   0.992896   0.039971 | -0.007109  -0.007138   0.019935
Body 3 :  0.020717   0.972888   0.972173 |  0.010727  -0.017153  -0.017876

Cycle 3
Body 1 :  0.031600   0.001625   0.000034 |  0.011072   0.001094   0.000033
Body 2 :  0.983985   0.983910   0.059834 | -0.010735  -0.010835   0.019790
Body 3 :  0.031643   0.953868   0.952235 |  0.011125  -0.020886  -0.021999

Cycle 4
Body 1 :  0.042858   0.002913   0.000081 |  0.011443   0.001481   0.000060
Body 2 :  0.971393   0.971163   0.079509 | -0.014448  -0.014659   0.019561
Body 3 :  0.042981   0.931039   0.928087 |  0.011552  -0.024772  -0.026299

Cycle 5
Body 1 :  0.054492   0.004595   0.000160 |  0.011826   0.001884   0.000097
Body 2 :  0.955030   0.954509   0.098909 | -0.018278  -0.018649   0.019238
Body 3 :  0.054766   0.904225   0.899522 |  0.012018  -0.028857  -0.030829

Cycle 6
Body 1 :  0.066517   0.006691   0.000281 |  0.012224   0.002308   0.000145
Body 2 :  0.934759   0.933760   0.117931 | -0.022265  -0.022849   0.018806
Body 3 :  0.067040   0.873197   0.866280 |  0.012530  -0.033199  -0.035655

Cycle 7
Body 1 :  0.078950   0.009225   0.000456 |  0.012642   0.002759   0.000206
Body 2 :  0.910400   0.908677   0.136456 | -0.026454  -0.027316   0.018244
Body 3 :  0.079856   0.837662   0.828023 |  0.013101  -0.037871  -0.040861

Cycle 8
Body 1 :  0.091815   0.012227   0.000702 |  0.013086   0.003245   0.000284
Body 2 :  0.881722   0.878958   0.154340 | -0.030902  -0.032122   0.017523
Body 3 :  0.093281   0.797239   0.784313 |  0.013749  -0.042975  -0.046559

Cycle 9
Body 1 :  0.105140   0.015737   0.001035 |  0.013564   0.003775   0.000383
Body 2 :  0.848429   0.844216   0.171401 | -0.035684  -0.037362   0.016600
Body 3 :  0.107405   0.751427   0.734579 |  0.014498  -0.048649  -0.052908

Cycle 10
Body 1 :  0.118964   0.019805   0.001481 |  0.014085   0.004362   0.000509
Body 2 :  0.810137   0.803953   0.187408 | -0.040900  -0.043166   0.015414
Body 3 :  0.122346   0.699554   0.678056 |  0.015384  -0.055097  -0.060138

Cycle 11
Body 1 :  0.133337   0.024498   0.002071 |  0.014662   0.005025   0.000672
Body 2 :  0.766343   0.757509   0.202050 | -0.046687  -0.049720   0.013868
Body 3 :  0.138268   0.640690   0.613685 |  0.016460  -0.062633  -0.068603

Cycle 12
Body 1 :  0.148327   0.029907   0.002851 |  0.015317   0.005792   0.000888
Body 2 :  0.716377   0.703998   0.214889 | -0.053246  -0.057302   0.011810
Body 3 :  0.155406   0.573482   0.539941 |  0.017816  -0.071782  -0.078886

Cycle 13
Body 1 :  0.164025   0.036157   0.003887 |  0.016079   0.006709   0.001184
Body 2 :  0.659310   0.642172   0.225282 | -0.060887  -0.066351   0.008976
Body 3 :  0.174112   0.495836   0.454475 |  0.019596  -0.083511  -0.092045

Cycle 14
Body 1 :  0.180564   0.043437   0.005286 |  0.017000   0.007852   0.001613
Body 2 :  0.593807   0.570186   0.232208 | -0.070119  -0.077621   0.004875
Body 3 :  0.194929   0.404136   0.353320 |  0.022038  -0.099890  -0.110265

Cycle 15
Body 1 :  0.198150   0.052049   0.007234 |  0.018171   0.009372   0.002283
Body 2 :  0.517817   0.485100   0.233878 | -0.081861  -0.092550  -0.001535
Body 3 :  0.218605   0.290860   0.228583 |  0.025314  -0.126661  -0.139210

Cycle 16
Body 1 :  0.217126   0.062542   0.010117 |  0.019781   0.011614   0.003484
Body 2 :  0.427899   0.381659   0.226654 | -0.097974  -0.114332  -0.012913
Body 3 :  0.244268   0.131956   0.057562 |  0.026013  -0.191148  -0.202831

Cycle 17
Body 1 :  0.238346   0.076539   0.015221 |  0.022658   0.016380   0.006723
Body 2 :  0.317489   0.248502   0.200967 | -0.122846  -0.151982  -0.038461
Body 3 :  0.075592  -0.559591  -0.487315 | -0.363366  -1.191945  -0.886924

Cycle 18
Body 1 :  0.263123   0.097523   0.026918 |  0.026898   0.025587   0.016672
Body 2 :  0.173428   0.050424   0.112716 | -0.165275  -0.244174  -0.138041
Body 3 : -0.286241  -1.745597  -1.369528 | -0.360299  -1.180066  -0.877501

Cycle 19
Body 1 :  0.270854   0.113045   0.061923 | -0.011436   0.005457   0.053339
Body 2 :  0.199821  -0.093105  -0.208666 |  0.218061  -0.042882  -0.504723
Body 3 : -0.646318  -2.924909  -2.246453 | -0.359856  -1.178559  -0.876350

Cycle 20
Body 1 :  0.258572   0.116046   0.112038 | -0.013129   0.000544   0.046890
Body 2 :  0.426346  -0.111425  -0.681150 |  0.234987   0.006241  -0.440245
Body 3 : -1.006089  -4.103186  -3.122591 | -0.359686  -1.177995  -0.875924

```



## J

<p>Although in general the n-body problem doesn’t have an explicit solution, certain configurations do, and these 'nice' configurations can be used to validate and perform unbiased comparisons of numeric solvers.  A fairly simple configuration with an exact explicit solution is a radially symmetric system of three bodies orbiting at constant velocity around a central axis:
<ul>Given:
<li>n = 3 <em>-- number of bodies</em></li>
<li>g = 1 <em>-- gravitational constant</em></li>
<li>m = 1 <em>-- mass per body</em></li>
<li>r = 1 <em>-- radius</em></li>
</ul><ul>Then:
<li>f = (1/3)^(1/2) <em>-- centripetal force</em></li>
<li>a = (1/3)^(1/2) <em>-- centripetal acceleration</em></li>
<li>v = (1/3)^(1/4) <em>-- velocity</em></li>
</ul></p>
<p>This is a variation on the [Klemperer rosette[https://en.wikipedia.org/wiki/Klemperer_rosette]]. </p>
<h4>Implementing the physics</h4>
<ul>
<li>The problem space is a collection of objects. </li>
<li>Object is { id, mass, position, velocity }. </li>
<li>Position is { x, y, z }. </li>
<li>Velocity is { vx, vy, vz }.</li>
</ul>

```J
g  =: 1
I  =:     0&{"1
M  =:     1&{"1
IM =:   0 1&{"1
D  =: 3 : 0"1
   2 3 4{y
:
   2 3 4{y-x
)
V  =: 5 6 7&{"1
D3 =: 4 : '(%:+/*:x D y)^3'"1
F  =: 3 : 0"1
   y F/y
:
   g*(M x)*(M y)*(y D x) % (x D3 y)
)
A  =: 3 : 0
   ff =. y F/y
   f =. +/ff
   f % (M y)
)
NEXT =: 4 : 0
   dt =. x
   im =. IM y
   p0  =. D y
   v0  =. V y
   f =.  +/(F/~y)
   a =. f%M y
   v1  =. v0 + dt * a
   p1  =. p0 + dt * (v0 + v1)%2
   z =. |: ((|: im),(|: p1),(|: v1))
   out =: out,D z
   z
)
```

<h4>Integrator</h4>
<p>Iterate over a time interval.  Plot the results (x and y coordinates over time). </p>

```J
dt =: 0.001
maxn =: 20000
require'plot'
ITER =: 3 : 0
   maxn ITER y
:
   out =: (0,(#y),3)$0
   dt NEXT^:x y
   plot 0 1 { |: out
)
```

<h4>Configurations</h4>
<h5>Configuration generator</h5>
<p>Generate a radially symmetrical configuration for a given number of bodies and an initial rotational velocity</p>

```J
require'trig'
GEN =: 3 : 0
   1 GEN y
:
   m =. 1
   r =. 1
   p =. r*(|:(2,y)$(cos,sin)(i.y)*2*pi%y),.0
   v =. x*(|:(2,y)$(cos,sin)(pi%2)+(i.y)*2*pi%y),.0
   (i.y),.(y#m),.p,.v
)

```

<h5>Generate a few cases.</h5>
<p>The 3-body static equilibrium case is proposed as the basis for validation, and for comparison of alternative methods.</p>

```J
cases =: 3 : 0
  case =. 3 : 'y;".y'
  r =. 0 2 $ a:
  r =. r, case 'GEN 4'
  r =. r, case '0.75 GEN 3'
  r =. r, case '((1%3)^(1%4)) GEN 3'
)
<[cases''
+------------------------------------------------------------------------------+
¦GEN 4              ¦0 1            1           0 0  6.12323e_17            1 0¦
¦                   ¦1 1  6.12323e_17           1 0           _1  1.22465e_16 0¦
¦                   ¦2 1           _1 1.22465e_16 0 _1.83697e_16           _1 0¦
¦                   ¦3 1 _1.83697e_16          _1 0            1 _2.44929e_16 0¦
+-------------------+----------------------------------------------------------¦
¦0.75 GEN 3         ¦0 1    1         0 0 4.59243e_17   0.75 0                 ¦
¦                   ¦1 1 _0.5  0.866025 0   _0.649519 _0.375 0                 ¦
¦                   ¦2 1 _0.5 _0.866025 0    0.649519 _0.375 0                 ¦
+-------------------+----------------------------------------------------------¦
¦((1%3)^(1%4)) GEN 3¦0 1    1         0 0 4.65265e_17  0.759836 0              ¦
¦                   ¦1 1 _0.5  0.866025 0   _0.658037 _0.379918 0              ¦
¦                   ¦2 1 _0.5 _0.866025 0    0.658037 _0.379918 0              ¦
+------------------------------------------------------------------------------+

```

<h5>Static equilibrium case. </h5>
<p> Bodies orbit with constant kinetic energy.</p>

```J
maxn ITER z0 =: ((1%3)^(1%4)) GEN 3
```

[<em>sinusoidal curves of constant amplitude</em>[https://commons.wikimedia.org/wiki/File:Nbody_j_2.jpg]]
<p>Determine orbital period, and compare configuration after two rotations vs. initial position. </p>

```J

v =: (1%3)^(1%4)
dt =: 0.001
<[rot2 =: (>. (4 * pi) % (v * dt))   NB. two rotations
+-----+
¦16539¦
+-----+
rot2 ITER v GEN 3
<[({.out)-{:out
+------------------------+
¦_0.00559932  0.0897287 0¦
| _0.0749077 _0.0497135 0¦
¦   0.080507 _0.0400152 0¦
+------------------------+

```

<h5>Dynamic equilibrium case. </h5>
<p> Within a narrow range of initial velocities, the bodies orbit, maintaining symetry and swapping kinetic and potential energy back and forth.  The ideal system is stable, but an integrating solver will not be, due to numerical precision. </p>

```J>maxn ITER z1 =: 0.6 GEN 3</lang

[<em>sinusoidal curves of varying amplitudes</em>  [https://commons.wikimedia.org/wiki/File:Nbody_j_1.jpg]]
<h5>Perturbed case. </h5>
<p>Any perturbation from symmetry, however small, will eventually cause the bodies to escape. Note that the effect demonstrated here is due to the physics of the problem, independently of the numerical precision and increment size of the solver. </p>

```J
maxn ITER z2 =: (0.001+6{0{z0)(<0 6)}z0
```

[<em>curves start out nice then get jumbled and diverge</em>  [https://commons.wikimedia.org/wiki/File:Nbody_j_3.jpg]]
<h5>n>3</h5>
<p>We can generate and execute tests for radially symmetrical configurations of any number of bodies, run the simulations, and compare the results with an explicitly determined result:</p>

```J
r  =. 4 : '%:+/*:(1-cos 2*pi*y%x),sin 2*pi*y%x'     NB. distance to nth body
f0 =. 4 : '1%((x r y)^2)'                           NB. force due to nth body
f  =. 4 : '(x f0 y)*(1-cos 2*pi*y%x)%x r y'"0       NB. centripetal component of force
a  =. 3 : '+/(y f i. y)'                            NB. centripetal acceleration
GENM =: 3 : 0"0                    NB. accumulated discrepancy over one rotation for n bodies
  N  =: y                          NB. number of bodies
  VS =: (a N)^0.5                  NB. velocity for static equilibrium
  TS =: 2*pi%VS                    NB. rotational time
  dt =: 0.001                      NB. time increment for simulation
  IS =: >.2*pi%VS*dt               NB. number of steps for simulation
  IS ITER VS GEN N                 NB. run the simulation
  E =: >./>./({.out)-{:out         NB. get max difference between initial and final state
  N;VS;TS;E                        NB. return the results
)
smoutput ' n';'velocity';' period';'    error'
smoutput GENM 3+i.17
+-----------------------------+
¦ n¦velocity¦ period¦    error¦
+--+--------+-------+---------¦
¦3 ¦0.759836¦8.26914¦0.0225829¦
+--+--------+-------+---------¦
¦4 ¦0.978318¦6.42243¦0.0293585¦
+--+--------+-------+---------¦
¦5 ¦1.17319 ¦5.35563¦0.0354237¦
+--+--------+-------+---------¦
¦6 ¦1.3518  ¦4.64803¦0.0400031¦
+--+--------+-------+---------¦
¦7 ¦1.51815 ¦4.13872¦0.0467118¦
+--+--------+-------+---------¦
¦8 ¦1.67477 ¦3.75166¦0.0505916¦
+--+--------+-------+---------¦
¦9 ¦1.82341 ¦3.44584¦0.0553926¦
+--+--------+-------+---------¦
¦10¦1.96531 ¦3.19704¦0.0586112¦
+--+--------+-------+---------¦
¦11¦2.10142 ¦2.98997¦0.0650293¦
+--+--------+-------+---------¦
¦12¦2.23248 ¦2.81445¦0.0669045¦
+--+--------+-------+---------¦
¦13¦2.35906 ¦2.66342¦0.0706303¦
+--+--------+-------+---------¦
¦14¦2.48166 ¦2.53185¦0.0763112¦
+--+--------+-------+---------¦
¦15¦2.60067 ¦2.41599¦0.0802535¦
+--+--------+-------+---------¦
¦16¦2.71641 ¦2.31305¦0.0802621¦
+--+--------+-------+---------¦
¦17¦2.82918 ¦2.22085¦0.0864611¦
+--+--------+-------+---------¦
¦18¦2.93922 ¦2.13771¦0.0898613¦
+--+--------+-------+---------¦
¦19¦3.04673 ¦2.06227¦0.0916438¦
+-----------------------------+
```

<h5>Sensitivity to discretization</h5>
<p>Although the static-equilibrium configuration is stable, its
simulation is not, due to discretization and numerical precision. We
can examine the sensitivity to time increment, taking position error after
one rotation as a stability metric:</p>

```J
GENDT =: 3 : 0"0
  dt =: y
  GENDT0 3
)
GENDT0 =: 3 : 0"0
  N=:y
  VS =: (a N)^0.5
  IS=:>.2*pi%VS*dt
  IS ITER VS GEN N
  E =: >./>./({.out)-{:out
  dt;E
)
smoutput 'dt    ';'error     '
smoutput (GENDT"0) 10^_4+i.5
+-----------------+
¦dt    ¦error     ¦
+-----------------+
¦0.0001¦0.00227764¦
+------+----------¦
¦0.001 ¦0.0225829 ¦
+------+----------¦
¦0.01  ¦0.228495  ¦
+------+----------¦
¦0.1   ¦1.54637   ¦
+------+----------¦
¦1     ¦6.0354    ¦
+-----------------+
```



## Java

{{trans|Kotlin}}

```Java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class NBodySim {
    private static class Vector3D {
        double x, y, z;

        public Vector3D(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        public Vector3D plus(Vector3D rhs) {
            return new Vector3D(x + rhs.x, y + rhs.y, z + rhs.z);
        }

        public Vector3D minus(Vector3D rhs) {
            return new Vector3D(x - rhs.x, y - rhs.y, z - rhs.z);
        }

        public Vector3D times(double s) {
            return new Vector3D(s * x, s * y, s * z);
        }

        public double mod() {
            return Math.sqrt(x * x + y * y + z * z);
        }
    }

    private static final Vector3D origin = new Vector3D(0, 0, 0);

    private static class NBody {
        private double gc;
        private int bodies;
        public final int timeSteps;
        private double[] masses;
        private Vector3D[] positions;
        private Vector3D[] velocities;
        private Vector3D[] accelerations;

        public NBody(String fileName) throws IOException {
            Path path = Paths.get(fileName);
            List<String> lines = Files.readAllLines(path);

            String[] gbt = lines.get(0).split(" ");
            gc = Double.parseDouble(gbt[0]);
            bodies = Integer.parseInt(gbt[1]);
            timeSteps = Integer.parseInt(gbt[2]);
            masses = new double[bodies];
            positions = new Vector3D[bodies];
            Arrays.fill(positions, origin);
            velocities = new Vector3D[bodies];
            Arrays.fill(velocities, origin);
            accelerations = new Vector3D[bodies];
            Arrays.fill(accelerations, origin);
            for (int i = 0; i < bodies; ++i) {
                masses[i] = Double.parseDouble(lines.get(i * 3 + 1));
                positions[i] = decompose(lines.get(i * 3 + 2));
                velocities[i] = decompose(lines.get(i * 3 + 3));
            }
            System.out.printf("Contents of %s\n", fileName);
            for (String line : lines) {
                System.out.println(line);
            }
            System.out.println();
            System.out.print("Body   :      x          y          z    |");
            System.out.println("     vx         vy         vz");
        }

        private Vector3D decompose(String line) {
            String[] xyz = line.split(" ");
            double x = Double.parseDouble(xyz[0]);
            double y = Double.parseDouble(xyz[1]);
            double z = Double.parseDouble(xyz[2]);
            return new Vector3D(x, y, z);
        }

        private void resolveCollisions() {
            for (int i = 0; i < bodies; ++i) {
                for (int j = i + 1; j < bodies; ++j) {
                    if (positions[i].x == positions[j].x
                        && positions[i].y == positions[j].y
                        && positions[i].z == positions[j].z) {
                        Vector3D temp = velocities[i];
                        velocities[i] = velocities[j];
                        velocities[j] = temp;
                    }
                }
            }
        }

        private void computeAccelerations() {
            for (int i = 0; i < bodies; ++i) {
                accelerations[i] = origin;
                for (int j = 0; j < bodies; ++j) {
                    if (i != j) {
                        double temp = gc * masses[j] / Math.pow((positions[i].minus(positions[j])).mod(), 3);
                        accelerations[i] = accelerations[i].plus(positions[j].minus(positions[i]).times(temp));
                    }
                }
            }
        }

        private void computeVelocities() {
            for (int i = 0; i < bodies; ++i) {
                velocities[i] = velocities[i].plus(accelerations[i]);
            }
        }

        private void computePositions() {
            for (int i = 0; i < bodies; ++i) {
                positions[i] = positions[i].plus(velocities[i]).plus(accelerations[i].times(0.5));
            }
        }

        public void simulate() {
            computeAccelerations();
            computePositions();
            computeVelocities();
            resolveCollisions();
        }

        public void printResults() {
            String fmt = "Body %d : % 8.6f  % 8.6f  % 8.6f | % 8.6f  % 8.6f  % 8.6f\n";
            for (int i = 0; i < bodies; ++i) {
                System.out.printf(
                    fmt,
                    i + 1,
                    positions[i].x, positions[i].y, positions[i].z,
                    velocities[i].x, velocities[i].y, velocities[i].z
                );
            }
        }
    }

    public static void main(String[] args) throws IOException {
        String filename = "nbody.txt";
        NBody nb = new NBody(filename);
        for (int i = 0; i < nb.timeSteps; ++i) {
            System.out.printf("\nCycle %s\n", i + 1);
            nb.simulate();
            nb.printResults();
        }
    }
}
```

{{out}}

```txt
Contents of nbody.txt
0.01 3 20
1
0 0 0
0.01 0 0
0.1
1 1 0
0 0 0.02
0.001
0 1 1
0.01 -0.01 -0.01

Body   :      x          y          z    |     vx         vy         vz

Cycle 1
Body 1 :  0.010177   0.000179   0.000002 |  0.010354   0.000357   0.000004
Body 2 :  0.998230   0.998232   0.020002 | -0.003539  -0.003536   0.020004
Body 3 :  0.010177   0.988232   0.988055 |  0.010354  -0.013536  -0.013889

Cycle 2
Body 1 :  0.020709   0.000718   0.000011 |  0.010710   0.000721   0.000014
Body 2 :  0.992907   0.992896   0.039971 | -0.007109  -0.007138   0.019935
Body 3 :  0.020717   0.972888   0.972173 |  0.010727  -0.017153  -0.017876

Cycle 3
Body 1 :  0.031600   0.001625   0.000034 |  0.011072   0.001094   0.000033
Body 2 :  0.983985   0.983910   0.059834 | -0.010735  -0.010835   0.019790
Body 3 :  0.031643   0.953868   0.952235 |  0.011125  -0.020886  -0.021999

Cycle 4
Body 1 :  0.042858   0.002913   0.000081 |  0.011443   0.001481   0.000060
Body 2 :  0.971393   0.971163   0.079509 | -0.014448  -0.014659   0.019561
Body 3 :  0.042981   0.931039   0.928087 |  0.011552  -0.024772  -0.026299

Cycle 5
Body 1 :  0.054492   0.004595   0.000160 |  0.011826   0.001884   0.000097
Body 2 :  0.955030   0.954509   0.098909 | -0.018278  -0.018649   0.019238
Body 3 :  0.054766   0.904225   0.899522 |  0.012018  -0.028857  -0.030829

Cycle 6
Body 1 :  0.066517   0.006691   0.000281 |  0.012224   0.002308   0.000145
Body 2 :  0.934759   0.933760   0.117931 | -0.022265  -0.022849   0.018806
Body 3 :  0.067040   0.873197   0.866280 |  0.012530  -0.033199  -0.035655

Cycle 7
Body 1 :  0.078950   0.009225   0.000456 |  0.012642   0.002759   0.000206
Body 2 :  0.910400   0.908677   0.136456 | -0.026454  -0.027316   0.018244
Body 3 :  0.079856   0.837662   0.828023 |  0.013101  -0.037871  -0.040861

Cycle 8
Body 1 :  0.091815   0.012227   0.000702 |  0.013086   0.003245   0.000284
Body 2 :  0.881722   0.878958   0.154340 | -0.030902  -0.032122   0.017523
Body 3 :  0.093281   0.797239   0.784313 |  0.013749  -0.042975  -0.046559

Cycle 9
Body 1 :  0.105140   0.015737   0.001035 |  0.013564   0.003775   0.000383
Body 2 :  0.848429   0.844216   0.171401 | -0.035684  -0.037362   0.016600
Body 3 :  0.107405   0.751427   0.734579 |  0.014498  -0.048649  -0.052908

Cycle 10
Body 1 :  0.118964   0.019805   0.001481 |  0.014085   0.004362   0.000509
Body 2 :  0.810137   0.803953   0.187408 | -0.040900  -0.043166   0.015414
Body 3 :  0.122346   0.699554   0.678056 |  0.015384  -0.055097  -0.060138

Cycle 11
Body 1 :  0.133337   0.024498   0.002071 |  0.014662   0.005025   0.000672
Body 2 :  0.766343   0.757509   0.202050 | -0.046687  -0.049720   0.013868
Body 3 :  0.138268   0.640690   0.613685 |  0.016460  -0.062633  -0.068603

Cycle 12
Body 1 :  0.148327   0.029907   0.002851 |  0.015317   0.005792   0.000888
Body 2 :  0.716377   0.703998   0.214889 | -0.053246  -0.057302   0.011810
Body 3 :  0.155406   0.573482   0.539941 |  0.017816  -0.071782  -0.078886

Cycle 13
Body 1 :  0.164025   0.036157   0.003887 |  0.016079   0.006709   0.001184
Body 2 :  0.659310   0.642172   0.225282 | -0.060887  -0.066351   0.008976
Body 3 :  0.174112   0.495836   0.454475 |  0.019596  -0.083511  -0.092045

Cycle 14
Body 1 :  0.180564   0.043437   0.005286 |  0.017000   0.007852   0.001613
Body 2 :  0.593807   0.570186   0.232208 | -0.070119  -0.077621   0.004875
Body 3 :  0.194929   0.404136   0.353320 |  0.022038  -0.099890  -0.110265

Cycle 15
Body 1 :  0.198150   0.052049   0.007234 |  0.018171   0.009372   0.002283
Body 2 :  0.517817   0.485100   0.233878 | -0.081861  -0.092550  -0.001535
Body 3 :  0.218605   0.290860   0.228583 |  0.025314  -0.126661  -0.139210

Cycle 16
Body 1 :  0.217126   0.062542   0.010117 |  0.019781   0.011614   0.003484
Body 2 :  0.427899   0.381659   0.226654 | -0.097974  -0.114332  -0.012913
Body 3 :  0.244268   0.131956   0.057562 |  0.026013  -0.191148  -0.202831

Cycle 17
Body 1 :  0.238346   0.076539   0.015221 |  0.022658   0.016380   0.006723
Body 2 :  0.317489   0.248502   0.200967 | -0.122846  -0.151982  -0.038461
Body 3 :  0.075592  -0.559591  -0.487315 | -0.363366  -1.191945  -0.886924

Cycle 18
Body 1 :  0.263123   0.097523   0.026918 |  0.026898   0.025587   0.016672
Body 2 :  0.173428   0.050424   0.112716 | -0.165275  -0.244174  -0.138041
Body 3 : -0.286241  -1.745597  -1.369528 | -0.360299  -1.180066  -0.877501

Cycle 19
Body 1 :  0.270854   0.113045   0.061923 | -0.011436   0.005457   0.053339
Body 2 :  0.199821  -0.093105  -0.208666 |  0.218061  -0.042882  -0.504723
Body 3 : -0.646318  -2.924909  -2.246453 | -0.359856  -1.178559  -0.876350

Cycle 20
Body 1 :  0.258572   0.116046   0.112038 | -0.013129   0.000544   0.046890
Body 2 :  0.426346  -0.111425  -0.681150 |  0.234987   0.006241  -0.440245
Body 3 : -1.006089  -4.103186  -3.122591 | -0.359686  -1.177995  -0.875924
```



## Julia

Uses the NBodySimulator module.

```julia
using StaticArrays, Plots, NBodySimulator

const stablebodies = [MassBody(SVector(0.0, 1.0, 0.0), SVector( 5.775e-6, 0.0, 0.0), 2.0),
                      MassBody(SVector(0.0,-1.0, 0.0), SVector(-5.775e-6, 0.0, 0.0), 2.0)]
const bodies = [
    MassBody(SVector(0.0, 1.0, 0.0), SVector( 5.775e-6, 0.0, 0.0), 2.0),
    MassBody(SVector(0.0,-1.0, 0.0), SVector(-5.775e-6, 0.0, 0.0), 2.0),
    MassBody(SVector(0.0, 4.5, 0.0), SVector(-2.5e-6, 0.0, 0.0), 1.0)
]

const G = 6.673e-11  # m^3/kg/s^2
const timespan = (0.0, 1111150.0)

function nbodysim(nbodies, tspan)
    system = GravitationalSystem(nbodies, G)
    simulation = NBodySimulation(system, tspan)
    run_simulation(simulation)
end

simresult = nbodysim(bodies, timespan)
plot(simresult)

```



## K

<p>The simulation is meant to be viewed in-browser, using a Javascript implementation of (a nice subset of) K, plus graphics primitives, here: [http://johnearnest.github.io/ok/ike/ike.html[http://johnearnest.github.io/ok/ike/ike.html]]; just paste the code into the code window and push the go button.  Playing with the parameters in this interactive and immediate environment gives a feel for the system beyond what you are likely to get from tables or graphs.</p>
<h4>configuration generator</h4>
<p>The configurations shown here are based on those in the J task.</p>

```K
sq:{pow[x;2]};sqrt:{pow[x;0.5]};pi:3.14159265   / math stuff
cs:{(cos 2*pi*x%y;sin 2*pi*x%y)}                / positions
sc:{(sin 2*pi*x%y;-cos 2*pi*x%y)}               / velocities
```

<h4>static equilibrium configuration</h4>
<p>The system is stable for an initial velocity for which centrifugal and gravitational forces are in balance</p>

```K
n::3;m::n#1;p::cs\:[!n;n];v::sc\:[!n;n]         / count, masses, positions, velocities
g::1;vs::pow[1%3;1%4];v*::vs                    / gravitational constant, initial velocity
t::0;dt::0.01;rot:2                             / time, time increment, rotational period
```

<h4>dynamic equilibrium</h4>
<p>Within a narrow range of initial velocities the system oscillates between more and less kinetic and potential energy.</p>

```K
/ as for static case, plus:
/    v*::0.7;rot::2
```

<h4>unstable</h4>
<p>The slightest perturbation from symmetry causes the system to become unstable.</p>

```K
/ as for static case, plus:
/    v+::(0 0;0 0;0 0.0001);rot:4   / wait for it ... wait for it ...
```

<h4>physics</h4>

```K
DV:{p[x]-p[y]};DS:{sqrt[+/sq'DV[x;y]]}            / nth body position vector and magnitude
A1:{$[x=y;0;g*m[y]*DV[x;y]%pow[DS[x;y];3]]}       / nth body centripetal acceleration component
A:{+/{A1/:[x;!n]}'!n}                             / total centripetal acceleration
tmax::2*pi*rot%vs                                 / duration of simulation
N:{[dt]                                           / increment
  v0:v;v+::dt*A[];p+::dt*0.5*v+v0                 / update velocities and positions
  t+::dt;$[t>tmax;do[msg::,"[";tick::{}];0]       / update time, detect end of time
}
```

<h4>events and graphics</h4>

```K
s::n#3                                            / size of sprite bitmap
sc:1.25;P::{w*0.5*1+(1%sc)*x}                     / display units
wh::(w;h);C::wh%2                                 / viewbox
RGB:("#f00";"#0f0";"#00f")                        / color palette
BW:("#000";"#fff")                                / monochrome palette
msg::,":"                                         / initial position marker
tick::{N[dt]}                                     / time increment callback
draw::{                                           / display callback
  r:(,:'P[p]-s%2),'`RGB,',:'(2#'s)#'!n            / plot the bodies
  r,:,(P[1 0];BW;~,/'+text@`i$msg)                / plot the initial position marker
}
```

<h4>3D</h4>
<p>Although the animation is limited to two dimensions, 
we can verify that the implementation really does support three, 
by changing the axis of rotation in the initialization function:</p>

```J
cs:{(0;cos 2*pi*x%y;sin 2*pi*x%y)}     / x-axis
sc:{(0;sin 2*pi*x%y;-cos 2*pi*x%y)}    / objects on screen move up and down

cs:{(cos 2*pi*x%y;0;sin 2*pi*x%y)}     / y-axis
sc:{(sin 2*pi*x%y;0;-cos 2*pi*x%y)}    / objects on screen move left and right

cs:{(cos 2*pi*x%y;sin 2*pi*x%y;0)}     / z-axis
sc:{(sin 2*pi*x%y;-cos 2*pi*x%y;0)}    / objects on screen go round and round
```



## Kotlin

{{trans|C}}

```scala
// version 1.2.0

import java.io.File
import kotlin.math.sqrt
import kotlin.math.pow

class Vector3D(val x: Double, val y: Double, val z: Double) {

    operator fun plus(v: Vector3D) = Vector3D(x + v.x, y + v.y, z + v.z)
    operator fun minus(v: Vector3D) = Vector3D(x - v.x, y - v.y, z - v.z)
    operator fun times(s: Double) = Vector3D(s * x, s * y, s * z)

    val mod = sqrt(x * x + y * y + z * z)
}

val origin = Vector3D(0.0, 0.0, 0.0)

class NBody(fileName: String) {

    val gc: Double
    val bodies: Int
    val timeSteps: Int
    val masses: DoubleArray
    val positions: Array<Vector3D>
    val velocities: Array<Vector3D>
    val accelerations: Array<Vector3D>

    init {
        val f = File(fileName)
        val lines = f.readLines()
        val (g, b, t) = lines[0].split(' ')
        gc = g.toDouble()
        bodies = b.toInt()
        timeSteps = t.toInt()
        masses = DoubleArray(bodies)
        positions = Array<Vector3D>(bodies) { origin }
        velocities = Array<Vector3D>(bodies) { origin }
        accelerations = Array<Vector3D>(bodies) { origin }
        for (i in 0 until bodies) {
            masses[i] = lines[i * 3 + 1].toDouble()
            positions[i] = decompose(lines[i * 3 + 2])
            velocities[i] = decompose(lines[i * 3 + 3])
        }
        println("Contents of $fileName")
        println(f.readText())
        print("Body   :      x          y          z    |")
        println("     vx         vy         vz")
    }

    private fun decompose(line: String): Vector3D {
        val (x, y, z) = line.split(' ').map { it.toDouble() }
        return Vector3D(x, y, z)
    }

    private fun resolveCollisions() {
        for (i in 0 until bodies) {
            for (j in i + 1 until bodies) {
                if (positions[i].x == positions[j].x &&
                    positions[i].y == positions[j].y &&
                    positions[i].z == positions[j].z) {
                    val temp = velocities[i]
                    velocities[i] = velocities[j]
                    velocities[j] = temp
                }
            }
        }
    }

    private fun computeAccelerations() {
        for (i in 0 until bodies) {
            accelerations[i] = origin
            for (j in 0 until bodies) {
                if (i != j) {
                    val temp = gc * masses[j] / (positions[i] - positions[j]).mod.pow(3)
                    accelerations[i] += (positions[j] - positions[i]) * temp
                }
            }
        }
    }
 
    private fun computeVelocities() {
        for (i in 0 until bodies) velocities[i] += accelerations[i]
    }

    private fun computePositions() {
        for (i in 0 until bodies) {
            positions[i] += velocities[i] + accelerations[i] * 0.5
        }
    }

    fun simulate() {
        computeAccelerations()
        computePositions()
        computeVelocities()
        resolveCollisions()
    }

    fun printResults() {
        val fmt = "Body %d : % 8.6f  % 8.6f  % 8.6f | % 8.6f  % 8.6f  % 8.6f"
        for (i in 0 until bodies) {
            println(fmt.format(
                i + 1,
                positions[i].x,
                positions[i].y,
                positions[i].z,
                velocities[i].x,
                velocities[i].y,
                velocities[i].z
            ))
        }
    }
} 

fun main(args: Array<String>) {
    val fileName = "nbody.txt"
    val nb = NBody(fileName)
    for (i in 1..nb.timeSteps) {
        println("\nCycle $i")
        nb.simulate()
        nb.printResults()
    }
}
```


{{out}}

```txt

Contents of nbody.txt
0.01 3 20
1
0 0 0
0.01 0 0
0.1
1 1 0
0 0 0.02
0.001
0 1 1
0.01 -0.01 -0.01

Body   :      x          y          z    |     vx         vy         vz

Cycle 1
Body 1 :  0.010177   0.000179   0.000002 |  0.010354   0.000357   0.000004
Body 2 :  0.998230   0.998232   0.020002 | -0.003539  -0.003536   0.020004
Body 3 :  0.010177   0.988232   0.988055 |  0.010354  -0.013536  -0.013889

Cycle 2
Body 1 :  0.020709   0.000718   0.000011 |  0.010710   0.000721   0.000014
Body 2 :  0.992907   0.992896   0.039971 | -0.007109  -0.007138   0.019935
Body 3 :  0.020717   0.972888   0.972173 |  0.010727  -0.017153  -0.017876

Cycle 3
Body 1 :  0.031600   0.001625   0.000034 |  0.011072   0.001094   0.000033
Body 2 :  0.983985   0.983910   0.059834 | -0.010735  -0.010835   0.019790
Body 3 :  0.031643   0.953868   0.952235 |  0.011125  -0.020886  -0.021999

Cycle 4
Body 1 :  0.042858   0.002913   0.000081 |  0.011443   0.001481   0.000060
Body 2 :  0.971393   0.971163   0.079509 | -0.014448  -0.014659   0.019561
Body 3 :  0.042981   0.931039   0.928087 |  0.011552  -0.024772  -0.026299

Cycle 5
Body 1 :  0.054492   0.004595   0.000160 |  0.011826   0.001884   0.000097
Body 2 :  0.955030   0.954509   0.098909 | -0.018278  -0.018649   0.019238
Body 3 :  0.054766   0.904225   0.899522 |  0.012018  -0.028857  -0.030829

Cycle 6
Body 1 :  0.066517   0.006691   0.000281 |  0.012224   0.002308   0.000145
Body 2 :  0.934759   0.933760   0.117931 | -0.022265  -0.022849   0.018806
Body 3 :  0.067040   0.873197   0.866280 |  0.012530  -0.033199  -0.035655

Cycle 7
Body 1 :  0.078950   0.009225   0.000456 |  0.012642   0.002759   0.000206
Body 2 :  0.910400   0.908677   0.136456 | -0.026454  -0.027316   0.018244
Body 3 :  0.079856   0.837662   0.828023 |  0.013101  -0.037871  -0.040861

Cycle 8
Body 1 :  0.091815   0.012227   0.000702 |  0.013086   0.003245   0.000284
Body 2 :  0.881722   0.878958   0.154340 | -0.030902  -0.032122   0.017523
Body 3 :  0.093281   0.797239   0.784313 |  0.013749  -0.042975  -0.046559

Cycle 9
Body 1 :  0.105140   0.015737   0.001035 |  0.013564   0.003775   0.000383
Body 2 :  0.848429   0.844216   0.171401 | -0.035684  -0.037362   0.016600
Body 3 :  0.107405   0.751427   0.734579 |  0.014498  -0.048649  -0.052908

Cycle 10
Body 1 :  0.118964   0.019805   0.001481 |  0.014085   0.004362   0.000509
Body 2 :  0.810137   0.803953   0.187408 | -0.040900  -0.043166   0.015414
Body 3 :  0.122346   0.699554   0.678056 |  0.015384  -0.055097  -0.060138

Cycle 11
Body 1 :  0.133337   0.024498   0.002071 |  0.014662   0.005025   0.000672
Body 2 :  0.766343   0.757509   0.202050 | -0.046687  -0.049720   0.013868
Body 3 :  0.138268   0.640690   0.613685 |  0.016460  -0.062633  -0.068603

Cycle 12
Body 1 :  0.148327   0.029907   0.002851 |  0.015317   0.005792   0.000888
Body 2 :  0.716377   0.703998   0.214889 | -0.053246  -0.057302   0.011810
Body 3 :  0.155406   0.573482   0.539941 |  0.017816  -0.071782  -0.078886

Cycle 13
Body 1 :  0.164025   0.036157   0.003887 |  0.016079   0.006709   0.001184
Body 2 :  0.659310   0.642172   0.225282 | -0.060887  -0.066351   0.008976
Body 3 :  0.174112   0.495836   0.454475 |  0.019596  -0.083511  -0.092045

Cycle 14
Body 1 :  0.180564   0.043437   0.005286 |  0.017000   0.007852   0.001613
Body 2 :  0.593807   0.570186   0.232208 | -0.070119  -0.077621   0.004875
Body 3 :  0.194929   0.404136   0.353320 |  0.022038  -0.099890  -0.110265

Cycle 15
Body 1 :  0.198150   0.052049   0.007234 |  0.018171   0.009372   0.002283
Body 2 :  0.517817   0.485100   0.233878 | -0.081861  -0.092550  -0.001535
Body 3 :  0.218605   0.290860   0.228583 |  0.025314  -0.126661  -0.139210

Cycle 16
Body 1 :  0.217126   0.062542   0.010117 |  0.019781   0.011614   0.003484
Body 2 :  0.427899   0.381659   0.226654 | -0.097974  -0.114332  -0.012913
Body 3 :  0.244268   0.131956   0.057562 |  0.026013  -0.191148  -0.202831

Cycle 17
Body 1 :  0.238346   0.076539   0.015221 |  0.022658   0.016380   0.006723
Body 2 :  0.317489   0.248502   0.200967 | -0.122846  -0.151982  -0.038461
Body 3 :  0.075592  -0.559591  -0.487315 | -0.363366  -1.191945  -0.886924

Cycle 18
Body 1 :  0.263123   0.097523   0.026918 |  0.026898   0.025587   0.016672
Body 2 :  0.173428   0.050424   0.112716 | -0.165275  -0.244174  -0.138041
Body 3 : -0.286241  -1.745597  -1.369528 | -0.360299  -1.180066  -0.877501

Cycle 19
Body 1 :  0.270854   0.113045   0.061923 | -0.011436   0.005457   0.053339
Body 2 :  0.199821  -0.093105  -0.208666 |  0.218061  -0.042882  -0.504723
Body 3 : -0.646318  -2.924909  -2.246453 | -0.359856  -1.178559  -0.876350

Cycle 20
Body 1 :  0.258572   0.116046   0.112038 | -0.013129   0.000544   0.046890
Body 2 :  0.426346  -0.111425  -0.681150 |  0.234987   0.006241  -0.440245
Body 3 : -1.006089  -4.103186  -3.122591 | -0.359686  -1.177995  -0.875924

```



## Octave

{{trans|Perl 6}}
We'll show only the positions in the output.


```octave
global G = 6.674e-11;   # gravitational constant
global au = 150e9;      # astronomical unit

global ma = 2e30;       # mass of the Sun
global mb = 6e24;       # mass of Earth
global mc = 7.34e22;    # mass of the Moon

function ret = ABCdot(ABC, t)
global G au ma mb mc 
ret = ABC;
ret(1:9) = ABC(10:18);

a = ABC(1:3);
b = ABC(4:6);
c = ABC(7:9);

ab = norm(a - b);
bc = norm(b - c);
ca = norm(c - a);

ret(10:12) = G*(mb/ab^3*(b - a) + mc/ca^3*(c - a));
ret(13:15) = G*(ma/ab^3*(a - b) + mc/bc^3*(c - b));
ret(16:18) = G*(ma/ca^3*(a - c) + mb/bc^3*(b - c));

endfunction;

seconds_in_a_month = 60*60*24*27;
seconds_in_a_year  = 60*60*24*365.25;
t = (0:0.05:1);
ABC = vec(
[
0 0 0                                                           # Sun position
au 0 0                                                          # Earth position
0.998*au 0 0                                                    # Moon position
0 0 0                                                           # Sun speed
0 2*pi*au/seconds_in_a_year 0                                   # Earth speed
0 2*pi*(au/seconds_in_a_year + 0.002*au/seconds_in_a_month) 0    # Moon speed
]'
);


disp(lsode('ABCdot',ABC, t)(1:20,1:9));

```

{{out}}

```txt
   0.0000e+00   0.0000e+00   0.0000e+00   1.5000e+11   0.0000e+00   0.0000e+00   1.4970e+11   0.0000e+00   0.0000e+00
   2.2520e-11   1.8822e-19   0.0000e+00   1.5000e+11   1.4933e+03   0.0000e+00   1.4970e+11   1.5337e+03   0.0000e+00
   9.0080e-11   1.3338e-18   0.0000e+00   1.5000e+11   2.9865e+03   0.0000e+00   1.4970e+11   3.0673e+03   0.0000e+00
   2.0268e-10   3.1665e-18   0.0000e+00   1.5000e+11   4.4798e+03   0.0000e+00   1.4970e+11   4.6010e+03   0.0000e+00
   3.6032e-10   6.7638e-18   0.0000e+00   1.5000e+11   5.9731e+03   0.0000e+00   1.4970e+11   6.1347e+03   0.0000e+00
   5.6300e-10   1.2128e-17   0.0000e+00   1.5000e+11   7.4663e+03   0.0000e+00   1.4970e+11   7.6683e+03   0.0000e+00
   8.1072e-10   1.9549e-17   0.0000e+00   1.5000e+11   8.9596e+03   0.0000e+00   1.4970e+11   9.2020e+03   0.0000e+00
   1.1035e-09   3.0116e-17   0.0000e+00   1.5000e+11   1.0453e+04   0.0000e+00   1.4970e+11   1.0736e+04   0.0000e+00
   1.4413e-09   4.3249e-17   0.0000e+00   1.5000e+11   1.1946e+04   0.0000e+00   1.4970e+11   1.2269e+04   0.0000e+00
   1.8241e-09   1.0985e-16   0.0000e+00   1.5000e+11   1.3439e+04   0.0000e+00   1.4970e+11   1.3803e+04   0.0000e+00
   2.2520e-09   1.8655e-16   0.0000e+00   1.5000e+11   1.4933e+04   0.0000e+00   1.4970e+11   1.5337e+04   0.0000e+00
   2.7249e-09   2.7013e-16   0.0000e+00   1.5000e+11   1.6426e+04   0.0000e+00   1.4970e+11   1.6870e+04   0.0000e+00
   3.2429e-09   3.6059e-16   0.0000e+00   1.5000e+11   1.7919e+04   0.0000e+00   1.4970e+11   1.8404e+04   0.0000e+00
   3.8059e-09   4.5794e-16   0.0000e+00   1.5000e+11   1.9412e+04   0.0000e+00   1.4970e+11   1.9938e+04   0.0000e+00
   4.4139e-09   5.6217e-16   0.0000e+00   1.5000e+11   2.0906e+04   0.0000e+00   1.4970e+11   2.1471e+04   0.0000e+00
   5.0670e-09   6.7328e-16   0.0000e+00   1.5000e+11   2.2399e+04   0.0000e+00   1.4970e+11   2.3005e+04   0.0000e+00
   5.7651e-09   7.9128e-16   0.0000e+00   1.5000e+11   2.3892e+04   0.0000e+00   1.4970e+11   2.4539e+04   0.0000e+00
   6.5083e-09   9.1616e-16   0.0000e+00   1.5000e+11   2.5386e+04   0.0000e+00   1.4970e+11   2.6072e+04   0.0000e+00
   7.2965e-09   1.0479e-15   0.0000e+00   1.5000e+11   2.6879e+04   0.0000e+00   1.4970e+11   2.7606e+04   0.0000e+00
   8.1297e-09   1.1866e-15   0.0000e+00   1.5000e+11   2.8372e+04   0.0000e+00   1.4970e+11   2.9140e+04   0.0000e+00
```



## Pascal

This was written in Turbo Pascal, prompted by an article in an astronomy magazine about the orbit of an asteroid about a planet about a sun. It expects to receive command-line parameters for the run, and wants to use the screen graphics to animate the result. Alas, the turbo .bgi protocol no longer works on modern computers that have screens with many more dots. It contains options to select different methods for computation. An interesting point is that if the central mass is fixed in position (rather than wobbling about the centre of mass, as it does), then the Trojan orbit positions are not stable.

```Pascal

{$N+ Crunch only the better sort of numbers, thanks.}
{$M 50000,24000,24000] {Stack,minheap,maxheap.}
Program Swirl; Uses Graph, Crt;
{   Calculates and displays the orbit of an asteroid around a sun and a planet,
 as described in the article by C.C. Foster, in Sky & Telescope, September 1994,
 which included the source code of a programme written in basic, so some changes here.

    The situation: the asteroid is at X = (AX,AY) with velocity V = (VAX,VAY)
 and suffers acceleration from the central sun and the steadily orbiting planet.
 If the acceleration at the start of a time step is A = (acX,acY)
  then X = (A.dt/2 + V)dt + X   at the end of a time step, A presumed constant.
   and V = A.dt + V             (so update X before altering V)
 But that is a first-order method. The whole point is that A is not constant.
 Accordingly, use this X as an estimate of the position at the end of a time step,
 and calculate a fresh A at the new position. Use the average of this and the
 first A to perform the actual step.
    The next step in the escalation is the classic fourth-order Runge-Kutta method
 that alas involves a lot of repetitious code, or else trickery with loops.
 Whichever, there are four evaluations per step: at the start, then using that to
 probe ahead a half step, then again a half step, then a full step probe, then
 a weighted average to perform the step. While this gives fifth-order accuracy
 for each step, and thus fourth order accuracy at the end of a sequence of steps,
 this is rather painful. This is to say that halving the step size would
 reduce the upper bound of the error in a single step by a factor of 2**5, and
 that for the result after a sequence of steps by a factor of 2**4. However,
 this requires an upper bound on the value of the higher order derivatives of
 the function over the entirety of the area of interest, and for this problem,
 it does not exist, for the acceleration is unbounded above as a particle approaches
 a mass. It varies by 1/r**2 and as r approaches zero, there can be surprises.
    Unfortunately, the next stages, say Milne's Predictor-Corrector (which has
 the advantage of maintaining  the recent history, just what procedure Apastron
 needs), followed by the extrapolation to zero stepsize methods of Bulirsch and
 Stoer require heavy administration.
    This programme uses a few tricks to enable a step size of 1 so that no actual
 multiplication need be made, and likewise takes G as being one. It is the work
 of moments to change the step size with these methods, but the difficulty lies
 in when to change the step size and by how much. Remember that with a large step,
 singularities can be stepped over without noticing them, whereas a small step
 involves not just more work but also accumulating round-off. The predictor-
 corrector methods offer a good check on the step size (if you bother to compare
 the difference between the predicted and corrected values) but require a lot
 of administrative effort when changing it. However, this is not all to the bad
 as you might as well add some checks that the step size is not being changed
 too enthusiastically while you're at it. The problem is that the step size is
 being changed according to the behaviour of the results, but the behaviour of
 the results depends on the step size as well as the current region.
    Rather than pursue this matter (and make the effort) I have retained the
 fixed size step (but perhaps an option sometime later?) to allow comparison
 with the original article in Sky & Telescope (and also to make things easy
 for procedure Apastron). For close comparison you will need to fix the central
 sun (i.e. Wobble is false) and use Euler's method. The article however uses
 units convenient to its programme: the planet is at radius 200, the asteroid
 at 100, both starting on the x-axis. The planet has a mass of 5 to the sun's
 mass of 70 (which is more like that of a companion sun than a planet), with
 a year of 920 steps and the asteroid has an initial velocity in the y-direction
 of 0.7. (See why you shouldn't use full stops as decimal points!)
    These conditions result in the asteroid having successive periods of 466,
 452, 477, 437, etc steps in the original article, and the invocation

    WANDER 920 0.0714285  0.5 0  0.0  1.1  r f

    meaning
      920 steps to the planet's orbit,
      a planetary mass of 5/70 of the sun
      asteroid's radial distance being half that of the planet
      at zero degrees (i.e. on the x-axis)
      zero radial velocity
      circular velocity of 1.1 (an estimate from trials) times that of a circular
         orbit at the asteroid's initial radius
      Runge-Kutta method
      Sun fixed at the centre of mass

    Is about as close as I can get, giving an asteroid period of 390 or so.
    But the orbit is very unstable. With Euler's method, the picture is even worse
 with the asteroid spiralling outwards. I think that this is due to too large a
 step size and a low-order method, as test runs with the planetary mass set zero
 behave better, provided that there are many steps to an asteroid orbit and that it
 doesn't pass close to a mass. StepSize control would help, but it would mean
 more programming effort. Alternatively, I could have blundered, but I've stared
 at this so long that if there is a blunder, I can't see it.
    I prefer to believe that the step size is the problem. In a different programme
 on a different machine, a circular orbit of 90 mins about the earth (just above
 the atmosphere) with full units for G, etc, and a step size of one second (thus
 no multiply) so that there were 5,400 steps to an orbit, Euler's method after
 one orbit had the radial distance 2% high and increasing steadily, whereas the
 2'nd order Euler's method with a two second step size (so the same number of
 evaluations, as two per step) and also no multiplication (i.e. (a + a')/2 *dt) gave
 an error of one part on 6,000,000 (the precision of the 32-bit floating point
 numbers on an IBM1130) and oscillating.
    On the other hand, a simple arrangement such as the asteroid in a Trojan
 orbit with respect to the planet works well enough. Here the asteroid is in the
 same orbit as the planet, but leading (or lagging) by sixty degrees, thus:

    WANDER 920 0.001 1 +60  0 1

    Test runs give an unstable orbit unless the planet is much smaller than 5/70
 of the sun's mass, and also require that the sun not be fixed to the centre of
 mass. Having lots more steps per orbit doesn't change this. Thus, even though
 the sun's wobble is small, it is necessary and anyway, physically correct.
 In the case of a more massive planet, the obvious extension is to three equal
 masses in mutual orbit and invoking symmetry, a nice circular orbit would have
 them spaced at a hundred and twenty degrees.
    Incidentally, no attempt is made to spot occasions when the asteroid's position
 is exactly that of some attracting mass, so zero divisions can cause dismay.
 Except that they won't happen as a result of a computed step unless you are very
 unlucky, especially if the floating-point hardware is available, with eighteen
 digits that must all coincide. On the other hand, you can easily specify an
 initial position that will provoke a division by zero so if that is what you
 have asked for, then that is what you will receive.}


{Perpetrated by R.N.McLean (whom God preserve), Victoria University, Feb. VMMI.}

 Type Grist = {$IFOPT N+} extended {$ELSE} real {$ENDIF};
 Type Chaff  = {$IFOPT N+} single   {$ELSE} real {$ENDIF};
 const esc = #27;
 var colour,lastcolour,xmax,ymax,txtheight: integer;
 var stretch: grist;
 Var Asitwas: Word;
 Type AnyString = string[80];

 Function Min(i,j: integer): integer; BEGIN if i <= j then min:=i else min:=j; END;
 Function Max(i,j: integer): integer; BEGIN if i >= j then max:=i else max:=j; END;
 Procedure Beep(f,t: integer); BEGIN Sound(f); delay(t); NoSound; END;
 Procedure Croak(Gasp: string);        {A lethal word.}
  BEGIN
   TextMode(Asitwas);
   WriteLn;
   Writeln(Gasp);
   HALT;
  END;

 Function MemGrab(Var p: pointer;s: word): boolean;
  BEGIN
   p:=nil;
   if s > 0 then GetMem(p,s);
   MemGrab:=p<>nil;
  END;
 Procedure MemDrop(Var p: pointer;s: word);
  BEGIN
   if (s > 0) and (p <> nil) then FreeMem(p,s);
   p:=nil;                              {Oh that all usage would first check!}
  END;

 FUNCTION Trim(S : anystring) : anystring;
 var L1,L2 : integer;
 BEGIN
   L1 := 1;
   WHILE (L1 <= LENGTH(S)) AND (S[L1] = ' ') DO INC(L1);
   L2 := LENGTH(S);
   WHILE (S[L2] = ' ') AND (L2 > L1) DO DEC(L2);
   IF L2 >= L1 THEN Trim := COPY(S,L1,L2 - L1 + 1) ELSE Trim := '';
 END; {Of Trim.}

 FUNCTION Ifmt(Digits : longint) : anystring;
  var  S : anystring;
  BEGIN
   STR(Digits,S);
   Ifmt := Trim(S);
  END; { Ifmt }

 FUNCTION Ffmt(Digits: grist; Width,Decimals: integer): anystring;
  var
   S : anystring;
   L : integer; { a finger }
  BEGIN
   if digits = 0 then begin Ffmt:='0'; exit; end; {Mumble.}
   IF Abs(Digits) < 0.0000001 THEN STR(Digits,S)
    ELSE STR(Digits:Width:Decimals,S);
   s:=trim(s);
   if copy(s,1,2) = '0.' then s:=copy(s,2,Length(s) - 1);
   if copy(s,1,3) = '-0.' then s:=concat('-',copy(s,3,Length(s) - 2));
   l:=Length(s);
   if pos('.',s) > 0 then while (l > 0) and (s[l] = '0') do l:=l - 1;
   if s[l] = '.' then l:=l - 1;         {No non-zero fractional part!}
   S := COPY(S,1,L);                    {Drop trailing boredom.}
   l:=Pos('.',S);                       {Avoid sequences such as 3.1415.}
   if l > 0 then s[l]:=chr(249);        {By using decimal points, please.}
   Ffmt := S;
  END; { Ffmt }

 Function Atan2(x,y: grist): grist;
  var a: grist;
  BEGIN
   if x = 0 then
    if y > 0 then atan2:=pi/2
     else if y < 0 then atan2:=-pi/2
      else atan2:=0
    else if x > 0 then if y >= 0 then atan2:=arctan(y/x) else atan2:=2*pi + arctan(y/x)
     else atan2:=pi - arctan(-y/x);
  END;

 Procedure PrepareTheCanvas;
  Var mode: integer;
  Var SavedResultCode: integer;
  Var ax,ay: word;
  Function BGIFound(Here: Anystring): boolean;
   Var driver: integer;
   BEGIN
    Driver:=Detect;
    InitGraph(driver,mode,Here);
    SavedResultCode:=GraphResult;      {Mumble!}
    BGIFound:=SavedResultCode = 0;     {Missing file gives -3.}
   END; {Of BGIFound.}
  Var aplace: anystring;
  Var Palette: paletteType;
  BEGIN
   Mode:=0;
   if not BGIFound('') then
    if not BGIFound('C:\TP7\BGI') then
     if not BGIFound('D:\TP4') then
      begin
       WriteLn;
       WriteLn('Trouble with Borland''s Graphic Interface!');
       WriteLn('Message: ',GraphErrorMsg(SavedResultCode));
       WriteLn('I''ve tried "", "C:\TP4", and "D:\TP4"...');
       Repeat
        Write('Another place to look? (e.g. a:\bgiset)');
        ReadLn(Aplace);         {As opposed to Read. weird.}
        If Aplace = '' then
         begin
          writeln;
          write('No .bgi, no piccy.');
          halt;
         end;
       until BGIFound(Aplace);
      end;
   SetViewPort(0,0,GetMaxX,GetMaxY,True); {Clipping on, thanks. No apparent slowdown.}
   SetGraphMode(Mode);
   GetPalette(Palette);
   LastColour := Palette.Size - 1; {Colours are 0:15, 16 thereof.}
   if LastColour <= 0 then LastColour:=1;
   xmax:=GetMaxX; ymax:=GetMaxY;
   txtheight:=TextHeight('I');
   GetAspectRatio(ax,ay); stretch:=ay/ax; {I hope this handles the screen's physical dimensions}
  END;                                    {as well as the number of dots in the x and y directions.}

 Procedure Splot(x,y:integer; bumf: anystring);
  var l: integer;
  BEGIN
   l:=TextWidth(Bumf);
   if x + l > xmax then x:=xmax - l;
   SetFillStyle(EmptyFill,Black); Bar(x,y,x + l,y + txtheight - 1);
   OutTextXY(x,y,bumf);
  END;

 Procedure FlashHint(msg: string);
  var i,h,ph,pl,y0,rt: integer;
  var l1,l2,nl: integer;
  var board: pointer; BoardSize: word;
  BEGIN
   nl:=0; pl:=0; l2:=1;
   repeat
    l1:=l2;
    while (l2 <= Length(msg)) and (msg[l2] <> '%') do l2:=l2 + 1;
    nl:=nl + 1; pl:=max(pl,TextWidth(Copy(Msg,l1,l2 - l1)));
    l2:=l2 + 1;
   until l2 > Length(msg);
   h:=TextHeight('Idiotic') + 1;
   ph:=1 + nl*h;
   y0:=GetMaxY div 6 + h;
   BoardSize:=ImageSize(0,0,pl,ph);
   If BoardSize <= 0 then begin Beep(6666,200); exit; end;
   if KeyPressed then exit;        {Last chance to cut and run.}
   if not memgrab(board,BoardSize) then
    begin
     OutTextXY(0,y0,'Memory shortage...');
     Beep(8888,200);
     exit; {Thus save on too many "if"s below.}
    end;
   GetImage(0,y0,pl,y0 + ph,board^);    {Doesn't check that Board is not nil!&^$#@!}
   SetFillStyle(EmptyFill,Black); Bar(0,y0,pl,y0 + ph);
   SetColor(White);
   i:=0; l2:=1;
   repeat
    l1:=l2;
    while (l2 <= Length(msg)) and (msg[l2] <> '%') do l2:=l2 + 1;
    OutTextXY(0,y0 + 1 + i*h,Copy(Msg,l1,l2 - l1));
    i:=i + 1; l2:=l2 + 1;
   until l2 > Length(msg);
   rt:=28 + Length(msg);     {Read time, more for longer messages.}
   i:=0; while not KeyPressed and (i < rt) do
    begin
     delay(100);
     i:=i + 1;
    end;
   PutImage(0,y0,board^,NormalPut);
   MemDrop(board,BoardSize);
  END;

 PROCEDURE LurkWith(Msg: string);
  Var t,w: integer;
  BEGIN
   w:=111;
   repeat
    for t:=0 to w do
     begin
      if KeyPressed then exit;
      delay(100);
     end;
    FlashHint(msg);
    w:=222;
   until KeyPressed;
  END;

 Function KeyFondle: char;             {Equivalent to ReadKey, except...}
  Var ticker: integer;                 { after a delay, it gives a hint.}
  BEGIN                                {Screen and keyboard are connected by a computer...}
   Ticker:=666;                        {A delay counter.}
   While not KeyPressed do             {If nothing has happened, }
    begin                              { twiddle my thumbs.}
     ticker:=ticker - 1;               {My patience is being exhausted.}
     if ticker > 0 then Delay(60)      {Another irretrievable loss.}
      else FlashHint('Press a key!');  {Hullo sailor!}
    end;                               {Eventually, a key is pressed.}
   KeyFondle:=ReadKey                  {Yum.}
  END;


 Var Wobble,Dotty: boolean;
 Var Style: integer;
 Var Year,PM,SM,AR,AA,ARV,ACV: grist;
 Var n: longint;
 Procedure WanderAlong;
  const suncolour = Yellow;
  const planetcolour = LightGreen;
  const asteroidcolour = White;
  const anglecolour = cyan;
  const periodcolour = LightRed;
  Const radiuscolour = LightBlue;
  Const KeplerColour = Magenta;
  Const EnergyColour = White;
  Const dt = 1;
  var
   W,P,T,
   SX,SY,RS,
   PX,PY,RP,
   AX,AY,VAX,VAY,acX,acY,
   DSX,DSY,DS2,DS3,
   DPX,DPY,DP2,DP3: grist;
  var cxp,cyp,sxp,syp,pxp,pyp,axp,ayp: integer;
  var bxp,txp,byp,typ: integer;
  var nturn: longint;
  var T1,T2: grist;
  var pAX,pAY,ppAX,ppAY,D2,pD2,ppD2: grist;
  Var nstep: longint;
  var stepwise,bored: boolean;

  Function AsteroidEnergy: grist; {The asteroid mass is considered divided out.}
   BEGIN                          {Thus, 1/2 mv**2 becomes 1/2 v**2.}
    AsteroidEnergy:=(VAX*VAX + VAY*VAY)/2
     - SM/sqrt((AX - SX)*(AX - SX) + (AY - SY)*(AY - SY))
     - PM/sqrt((AX - PX)*(AX - PX) + (AY - PY)*(AY - PY));
   END;                           {Likewise, potential Energy = -G*M1*M2/r12 }

  Procedure SplotStep;
   BEGIN
    SetColor(LightBlue);
    Splot(0,0,'Steps: ' + ifmt(nstep));
    Splot(4*cxp div 3,0,'R='+ffmt(sqrt(AX*AX + AY*AY),8,2));
    SetColor(EnergyColour);
    Splot(4*cxp div 3,Txtheight,'E/m: '+ ffmt(AsteroidEnergy,8,2) + ' ');
   END;

  const types = 5;
  Const tummy = 666;
  var v: array[1..types,1..tummy] of Chaff;
  var vmax,vmin,vs: array[1..types] of Chaff;
  var show,varies: array[1..types] of boolean;
  const vcolour: array[1..types] of byte = (anglecolour,periodcolour,radiuscolour,KeplerColour,EnergyColour);
  var nv: integer;

  Procedure Splash;
   var i,j: integer;
   BEGIN
    SetFillStyle(EmptyFill,Black); Bar(bxp,byp,txp,typ);
    SetColor(Green);
    Line(bxp,byp,txp,byp); line(txp,byp,txp,typ);
    Line(txp,typ,bxp,typ); Line(bxp,typ,bxp,byp);
    for i:=1 to types do
     if show[i] and varies[i] then
      if dotty then for j:=1 to nv-1 do putpixel(bxp + round((v[i,j] - vmin[i])*vs[i]),byp+j,vcolour[i])
       else
        begin
         setcolor(vcolour[i]);
         MoveTo(bxp + round((v[i,1] - vmin[i])*vs[i]),byp+1);
         for j:=2 to nv-1 do LineTo(bxp + round((v[i,j] - vmin[i])*vs[i]),byp+j);
        end;
   END;

  Procedure DealWith(ch: char);
   var redraw: boolean;
   BEGIN
    Case upcase(ch) of
     ESC: bored:=true;
     'J': Dotty:=not Dotty;
     'A': show[1]:=not show[1];
     'P': show[2]:=not show[2];
     'R': show[3]:=not show[3];
     'K': show[4]:=not show[4];
     'E': show[5]:=not show[5];
     'S':stepwise:=not stepwise;
     '?':FlashHint('Pressing a key may help.%APRK or E selects dots to plot%S for Stepwise%ESC to quit.');
    end;
    if pos(upcase(ch),'JAPRKE') > 0 then splash;
   END;

  Procedure Swallow(v1,v2,v3,v4,v5: grist);
   var i,j,l: integer;
   var hic,flux: boolean;
   BEGIN
    flux:=nv = 3;
    if (nv >= typ - byp) or (nv >= tummy) then
     begin
      flux:=true;
      l:=nv div 3;
      for i:=1 to types do
       for j:=l+1 to nv do v[i,j - l]:=v[i,j];
      nv:=nv - l;
      for i:=1 to types do begin vmin[i]:=v[i,1]; vmax[i]:=v[i,1]; end;
      for i:=1 to types do
       for j:=2 to nv do
        begin
         if v[i,j] > vmax[i] then vmax[i]:=v[i,j];
         if v[i,j] < vmin[i] then vmin[i]:=v[i,j];
        end;
      for i:=1 to types do
       begin
        varies[i]:=vmax[i] <> vmin[i];
        if varies[i] then vs[i]:=(txp - bxp)/(vmax[i] - vmin[i]);
       end;
     end;
    nv:=nv + 1;
    v[1,nv]:=v1; v[2,nv]:=v2; v[3,nv]:=v3; v[4,nv]:=v4; v[5,nv]:=v5;
    if nv = 1 then for i:=1 to types do begin varies[i]:=false; vmax[i]:=v[i,1]; vmin[i]:=v[i,1]; end
     else for i:=1 to types do
      begin
       hic:=false;
       if v[i,nv] > vmax[i] then begin vmax[i]:=v[i,nv]; hic:=true; end;
       if v[i,nv] < vmin[i] then begin vmin[i]:=v[i,nv]; hic:=true; end;
       if hic then vs[i]:=(txp - bxp)/(vmax[i] - vmin[i]);
       if not varies[i] then varies[i]:=vmax[i] <> vmin[i];
       flux:=flux or hic;
      end;
    if nv > 3 then
     begin
      if flux then splash;
      for i:=1 to types do
       if show[i] and varies[i] then
        if dotty then putpixel(bxp + round((v[i,nv] - vmin[i])*vs[i]),byp+nv,vcolour[i])
         else
          begin
           setcolor(vcolour[i]);
           Line(bxp + round((v[i,nv-1] - vmin[i])*vs[i]),byp+nv-1,bxp + round((v[i,nv] - vmin[i])*vs[i]),byp+nv);
          end;
     end;
   END; {of Swallow.}
  Procedure Apastron;
  {Given: at the last three time steps, DS2, pD2, and ppD2, being (squares of)
   the distances from the centre of mass, and further, DS2 < pD2, pD2 > ppD2.
   Wanted: the time of greatest distance from the sun.
   So, fit a parabola to the three equally spaced values ppD2, pD2, DS2,
   differentiate and set to zero for the extremum, solving for x.
   y = ax**2 + bx + c, and why not have x = -1, 0, +1
   Thus c = y0,                  y-1 is ppD2  so c = pD2
        b = (y1 - y-1)/2         y0  is pD2      b = (DS2 - ppD2)/2
        a = (y1 + y-1 - 2y0)/2   y1  is DS2.     a = (DS2 + ppD2 - 2pDS2)/2
   The extremum is when 2ax + b = 0
    so x = -b/2a when y is an extremum, call this h.
   and x = 0 corresponds to the middle time, which was one back, so T-dt
   Having found the time of greatest distance, a small adjustment from T
   gives the location of the asteroid then (only a small change from time T)
   and thus the direction of the apastron. Since the acceleration is at right
   angles to the velocity it won't have much effect on the angle, and being
   minimal, not much effect on the distance. But, I'm not doing the arithmetic.}
   Var X,Y,a,r,h,k,e: grist;
   BEGIN
    {splot(0,20,'ppD2:'+ffmt(ppD2,15,2)+'  ');}
    {splot(0,30,' pD2:'+ffmt(pD2,15,3)+'> ');}
    {splot(0,40,'  D2:'+ffmt(D2,15,3)+'  ');}
    h:=(ppD2 - DS2)/(DS2 + ppD2 - 2*pD2) /2;
    {splot(0,50,'   h:'+ffmt(h,15,3+'  '));}
    t2:=T + (h - 1){*dt};
    x:=((AX + ppAX - 2*pAX)/2*h + (AX - ppAX)/2)*h + pAX;
    y:=((AY + ppAY - 2*pAY)/2*h + (AY - ppAY)/2)*h + pAY;
    r:=sqrt(x*x + y*y);
    SetColor(AngleColour); Line(cxp,cyp, cxp + round(x),cyp - round(y/stretch));
    a:=atan2(x,y)*180/pi;
    nturn:=nturn + 1;
    h:=(t2 - t1)/P*Year;               {Use the planet's 'year'.}
    k:=8*r*r*r/h/h;                    {Kepler's law: G(m + m')/4pi = d**3/t**2}
    e:=(VAX*VAX + VAY*VAY)/2
     - SM/sqrt((x - SX)*(x - SX) + (y - SY)*(y - SY))
     - PM/sqrt((x - PX)*(x - PX) + (y - PY)*(y - PY));
    Splot(0,0 + 1*TxtHeight,'Orbits: ' + ifmt(nturn));
    SetColor(AngleColour);  Splot(0,ymax - 1*txtheight,'Apastron: ' + ffmt(a,8,3) + 'ø   ');
    SetColor(PeriodColour); Splot(0,ymax - 2*txtheight,'Period: ' + ffmt(h,12,3)+' ');
    SetColor(RadiusColour); Splot(cxp*4 div 3,ymax - 1*txtheight,'Rmax: ' + ffmt(r,12,4)+' ');
    SetColor(KeplerColour); Splot(cxp*4 div 3,ymax - 2*txtheight,'Kepler: ' + ffmt(k,12,3)+' ');
    Swallow(a,h,r,k,e);
    t1:=t2;
   END; {of Apastron.}

  Const Methodname: array[1..3] of string = ('Euler','Euler2','RK4');
  var nSX,nSY,nPX,nPY,nAX,nAY: grist;
  Procedure EulerStep;                        {First-order.}
   BEGIN
    DSX:=SX - AX;                             DSY:=SY - AY;
    DS2:=DSX*DSX + DSY*DSY;                   DS3:=sqrt(DS2)*DS2;
    DPX:=PX - AX;                             DPY:=PY - AY;
    DP2:=DPX*DPX + DPY*DPY;                   DP3:=sqrt(DP2)*DP2;
    acX:=SM*DSX/DS3 + PM*DPX/DP3;             acY:=SM*DSY/DS3 + PM*DPY/DP3;
   END;
  Procedure SecondOrder;                      {Modified Euler, 2nd order predictor-corrector, etc..}
   BEGIN
    EulerStep;
    nAX:=AX + (0.5*acX{*dt} + VAX){*dt};      nAY:=AY + (0.5*acY{*dt} + VAY){*dt};
    DSX:=nSX - nAX;                           DSY:=nSY - nAY;
    DS2:=DSX*DSX + DSY*DSY;                   DS3:=sqrt(DS2)*DS2;
    DPX:=nPX - nAX;                           DPY:=nPY - nAY;
    DP2:=DPX*DPX + DPY*DPY;                   DP3:=sqrt(DP2)*DP2;
    acX:=(acX + SM*DSX/DS3 + PM*DPX/DP3)/2;   acY:=(acY + SM*DSY/DS3 + PM*DPY/DP3)/2;
   END;
  Procedure RungeKutta4;                      {The classic.}
   var h2,hsx,hsy,hpx,hpy,tax,tay,k1x,k1y,k2x,k2y,k3x,k3y: grist;
   BEGIN
    EulerStep;                                {The story at 0.}
    h2:=dt/2;                                 {Half a time step.}
    hSX:=(SX + nSX)/2; hSY:=(SY + nSY)/2;     {I'm not calling for a fresh sin and cos.}
    hPX:=(PX + nPX)/2; hPY:=(PY + nPY)/2;     {Halfway will do.}
    tAX:=AX + (0.5*acX*h2 + VAX)*h2;          tAY:=AY + (0.5*acY*h2 + VAY)*h2;
    DSX:=hSX - tAX;                           DSY:=hSY - tAY;
    DS2:=DSX*DSX + DSY*DSY;                   DS3:=sqrt(DS2)*DS2;
    DPX:=hPX - tAX;                           DPY:=hPY - tAY;
    DP2:=DPX*DPX + DPY*DPY;                   DP3:=sqrt(DP2)*DP2;
    k1x:=SM*DSX/DS3 + PM*DPX/DP3;             k1y:=SM*DSY/DS3 + PM*DPY/DP3;
    tAX:=AX + (0.5*k1X*h2 + VAX)*h2;          tAY:=AY + (0.5*k1Y*h2 + VAY)*h2;
    DSX:=hSX - tAX;                           DSY:=hSY - tAY;
    DS2:=DSX*DSX + DSY*DSY;                   DS3:=sqrt(DS2)*DS2;
    DPX:=hPX - tAX;                           DPY:=hPY - tAY;
    DP2:=DPX*DPX + DPY*DPY;                   DP3:=sqrt(DP2)*DP2;
    k2x:=SM*DSX/DS3 + PM*DPX/DP3;             k2y:=SM*DSY/DS3 + PM*DPY/DP3;
    tAX:=AX + (0.5*k2X{*dt} + VAX){*dt};      tAY:=AY + (0.5*k2Y{*dt} + VAY){*dt};
    DSX:=nSX - tAX;                           DSY:=nSY - tAY;
    DS2:=DSX*DSX + DSY*DSY;                   DS3:=sqrt(DS2)*DS2;
    DPX:=nPX - tAX;                           DPY:=nPY - tAY;
    DP2:=DPX*DPX + DPY*DPY;                   DP3:=sqrt(DP2)*DP2;
    k3x:=SM*DSX/DS3 + PM*DPX/DP3;             k3y:=SM*DSY/DS3 + PM*DPY/DP3;
    acX:=(acX + 2*(k1x + k2x) + k3x)/6;       acY:=(acY + 2*(k1y + k2y) + k3y)/6;
   END;
  Const BlobSize = 8;
  var r,vcirc: grist;
  var d,m,distant2: grist;
  var coswt,sinwt: grist;
  var rsp,rpp: integer;                {Radius of Sun in Pixels, and of Planet.}
  var pspix,pppix,pswas,ppwas: pointer;{Points to Sun pix, Planet pix.}
  var szspix,szppix: word;             {Sizes of the pix map.}
  var i1,i2: integer;
  Var ch: char;
  Var text: string;
  BEGIN
   bored:=false;
   Dotty:=true;
   Stepwise:=false;
   for i1:=1 to types do show[i1]:=true;
   bxp:=xmax; pyp:=round(ymax*stretch);
   if pyp < bxp then bxp:=pyp;         {Oh for square dots!##$%$#!}
   cxp:=bxp div 2; cyp:=round(bxp/stretch/2); {The centre of mass on the screen.}
   bxp:=bxp + 1; txp:=xmax;            {Base and top of a side box.}
   byp:=0;       typ:=ymax;            {Wherein will appear a plot.}
   nv:=0;                              {No values saved for the box.}
   R:=cxp - blobsize;                  {Available space on the screen, with safety.}
   RP:=R;                              {All for the planet's radius.}
   RS:=0;                              {If the sun is motionless.}
   M:=SM + PM;                         {Total mass of the system.}
   if pm = 0 then wobble:=false;       {The asteroid is as if massless.}
   if wobble then                      {But if the sun wobbles, }
    begin                              { then a re-adjustment.}
     RS:=R*PM/M;                       {Both orbit about the centre of mass.}
     RP:=R*SM/M;                       {RS + RP = R.}
     t:=RS; if RP > t then t:=RP;      {Re-scale to fill the screen.}
     RS:=RS*R/t; RP:=RP*R/t;
    end;
   D:=RS + RP;                         {Distance between planet and sun.}
   Distant2:=6*D*D;                    {A long way out.}
   M:=4*pi*pi*D*D*D/({G}year*year) /M; {Use the proper orbital period, with G = 1.}
   SM:=SM*M; PM:=PM*M; M:=SM + PM;     {Rescale so as to allow dt = 1.}
   P:=year;
   W:=2*Pi/P;                          {2Pi radians in one orbit, needing time P}
  {dt:=P/year;                         {'Year' Timesteps complete one P.}
   rsp:=Round(BlobSize*Sqrt(SM/M)); rpp:=Round(BlobSize*Sqrt(PM/M));
   if rsp < 1 then rsp:=1;          if rpp < 1 then rpp:=1;
                   {Prepare a solar disc.}
   sxp:=cxp - round(RS); syp:=cyp - 0;
   szspix:=ImageSize(sxp-rsp,syp-rsp,sxp+rsp,syp+rsp);
   if not(MemGrab(pspix,szspix) and MemGrab(pswas,szspix)) then croak('Can''t grab memory for the solar disc.');
   GetImage(sxp-rsp,syp-rsp,sxp+rsp,syp+rsp,pswas^);
   SetColor(SunColour); Circle(sxp,syp,rsp);
   SetFillStyle(SolidFill,SunColour); FloodFill(sxp,syp,SunColour);
   GetImage(sxp-rsp,syp-rsp,sxp+rsp,syp+rsp,pspix^);
   PutImage(sxp-rsp,syp-rsp,pswas^,0); {Remove the sun during preparation.}
                   {Prepare a planetary disc.}
   pxp:=cxp + round(RP);  pyp:=cyp + 0;
   szppix:=ImageSize(pxp-rpp,pyp-rpp,pxp+rpp,pyp+rpp);
   If not(MemGrab(pppix,szppix) and MemGrab(ppwas,szppix)) then Croak('Can''t grab memory for the planetary disc.');
   GetImage(pxp-rpp,pyp-rpp,pxp+rpp,pyp+rpp,ppwas^);
   SetColor(PlanetColour); Circle(pxp,pyp,rpp);
   SetFillStyle(SolidFill,PlanetColour); {FloodFill(pxp,pyp,PlanetColour);}
   GetImage(pxp-rpp,pyp-rpp,pxp+rpp,pyp+rpp,pppix^);
   PutImage(pxp-rpp,pyp-rpp,ppwas^,0);
                   {Place some geometry.}
   SetColor(1); Line(cxp - round(rp),cyp,cxp + round(rp),cyp);
                Line(cxp,cyp + round(rp/stretch),cxp,cyp - round(rp/stretch));
                Circle(cxp,cyp,Round(RP));
   SetColor(5); Line(cxp-1,cyp,cxp+1,cyp); Line(cxp,cyp-1,cxp,cyp+1);
   GetImage(sxp-rsp,syp-rsp,sxp+rsp,syp+rsp,pswas^); {+Geometry, no sun.}
   GetImage(pxp-rpp,pyp-rpp,pxp+rpp,pyp+rpp,ppwas^); {+Geometry, no planet.}
   PutImage(sxp-rsp,syp-rsp,pspix^,0);
   SetColor(LightBlue); Splot(0,0+2*txtheight,'Method: ' + methodname[style]);
   if wobble then text:='Wobble' else text:='Fixed';
   setcolor(SunColour); Splot(0,0+3*txtheight,text);

   SX:=0;             SY:=0;           {Place the sun at the centre of mass.}
   PX:=RP;            PY:=0;           {Place the planet on the x-axis.}
   if wobble then SX:=-RS;             {Opposite sides.}
   nSX:=SX;          nSY:=SY;          {In case it doesn't wobble.}
   axp:=0;           ayp:=0;           {Previous asteroid place.}
   D:=RP*AR;                           {Distance out from the C of M.}
   AX:=D*Cos(AA); AY:=D*Sin(AA);
   VCirc:=D*2*Pi/Sqrt(4*pi*pi*D*D*D/M);  {Velocity of circular orbit.}
   VAX:=VCirc*(-ACV*Sin(AA) + ARV*Cos(AA));
   VAY:=VCirc*(+ACV*Cos(AA) + ARV*Sin(AA));
   pAX:=AX; ppAX:=AX; pAY:=AY;ppAY:=AY;{Shouldn't be needed, but fear uninitialisation.}
   D2:=AX*AX + AY*AY;                  {Distance from the centre of mass.}
   pD2:=D2;ppD2:=D2;                   {We start at the Apastron.}
   T1:=0;
   nturn:=0;
   nstep:=0;
   {The story at time T, the start of a time step dt:
    The sun's mass SM at (SX,SY) and (nSX,nSY) at the end of the time step.
    The planet     PM    (PX,PY)     (nPX,nPY)
    The asteroid         (AX,AY) velocity (VAX,VAY)
    To compute its acceleration (acX,acY)
    and therefrom a new value for (AX,AY) and (VAX,VAY).}

   repeat
    nstep:=nstep + 1;                  {Well, it is about to be made.}
    t:=nstep{*dt};                     {This time is after the step has been made.}
    CosWT:=Cos(W*T); SinWT:=Sin(W*T);  {The compiler probably does a poor job.}
    nPX:=RP*Coswt;nPY:=RP*sinwt;       {And at the end of the step.}
    if wobble then                     {In proper centre-of-mass arrangements?}
     begin                             {Yes. The sun is wobbling.}
      nSX:=-RS*CosWT; nSY:=-RS*SinWT;  {And at the end of the time step.}
     end;                              {Enough fooling around. Now for the real task.}
    Case style of
    1:EulerStep;
    2:SecondOrder;
    3:RungeKutta4;
    end;
    AX:=AX + (0.5*acX{*dt} + VAX){*dt};       AY:=AY + (0.5*acY{*dt} + VAY){*dt};
    VAX:=VAX + acX{*dt};                      VAY:=VAY + acY{*dt};
    PX:=nPX;                                  PY:=nPY;
    if wobble then begin SX:=nSX; SY:=nSY; end;  {Thus attain the end of the time step.}
    PutPixel(axp,ayp,0);                      {Scrub the old asteroidal spot.}
    axp:=cxp + round(AX);                     ayp:=cyp - Round(AY/stretch);
    PutPixel(axp,ayp,AsteroidColour);         {Place the new asteroidal spot.}
    PutImage(pxp-rpp,pyp-rpp,ppwas^,0);       {Scrub the old planetary spot.}
    pxp:=cxp + round(PX);                     pyp:=cyp - round(PY/stretch);
    GetImage(pxp-rpp,pyp-rpp,pxp+rpp,pyp+rpp,ppwas^);
    PutImage(pxp-rpp,pyp-rpp,pppix^,0);       {Place the new planetary spot.}
    if wobble then                            {The sun might be mobile, too.}
     begin                                    {So see if it has moved noticeably.}
      i1:=cxp + round(SX);                    i2:=cyp - round(SY/stretch);
      if (i1 <> sxp) or (i2 <> syp) then      {At least a whole dot in some direction?}
       begin                                  {Yep.}
        PutImage(sxp-rsp,syp-rsp,pswas^,0);   {Restore what was there.}
        sxp:=i1; syp:=i2;                     {And jump to the new position.}
        GetImage(sxp-rsp,syp-rsp,sxp+rsp,syp+rsp,pswas^);
        PutImage(sxp-rsp,syp-rsp,pspix^,0);   {Splot.}
       end;                                   {So much for a noticeable move.}
     end;                                     {So much for sun movement.}
    D2:=AX*AX + AY*AY;                 {The new position.}
    if (D2 < pD2) and (pD2 > ppD2) then Apastron;
    ppAX:=pAX; ppAY:=pAY; pAX:=AX; pAY:=AY; ppD2:=pD2; pD2:=D2;
    if stepwise or (nstep mod 100 = 0) then splotstep;
    if D2 > Distant2 then              {Too distant for close interactions?}
     if AsteroidEnergy > 0 then        {Yeah, perhaps a getaway is in progress.}
      begin
       SplotStep;
       Splot(cxp,cyp div 3,'Escaping!');
       Bored:=true;
      end;
    if stepwise or keypressed then dealwith(KeyFondle);
   until bored;

   repeat
    Text:='';
    if nv > 3 then text:='Adjust dots, or%';
    text:=text + 'ESC to quit.';
    LurkWith(Text);
    ch:=KeyFondle;
    if ch <> esc then dealwith(ch);
   until ch = esc;

   MemDrop(pspix,szspix); MemDrop(pswas,szspix);
   MemDrop(pppix,szppix); MemDrop(ppwas,szppix);
  END; {Of WanderAlong.}

 Procedure Grunt(n: integer);
  Var i: integer;
  Var Unflushed: boolean;
  Procedure Z(Text: string);      {Roll some text.}
   BEGIN                          {With screen pauses.}
    if unflushed then ClrEol;     {Perhaps bumf lurks on this line.}
    WriteLn(Text); i:=i + 1;      {Writes only to the end of text, not eol.}
    if i >= Hi(WindMax) then      {Have we reached the bottom?}
     begin                        {Yes, the display would soon scroll up.}
      if unflushed then clreol;   {A last remnant.}
      unflushed:=false;           {Once scrolling starts, new lines are blank.}
      Write('(Press a key)');     {A hint, offering out-by-one possibilities.}
      if ReadKey = #0 then if ReadKey = esc then;      {Ignore a key.}
      GoToXY(1,wherey); ClrEol;   {Scrub the hint.}
      i:=0;                       {Restart the count.}
     end;                         {So much for a screen full.}
   END;                           {So much for that line.}
  BEGIN
   i:=n; Unflushed:=true;
   Z('   A massive planet is in a fixed circular orbit about its sun.');
   Z('Between it and the sun is an asteroid that wanders as it can.');
   Z('Although this problem is (just) within the scope of analytic methods,');
   Z('the asteroid''s movement is computed by numerical integration, with one');
   Z('step per ''day'' of the planet''s orbital period. As the simulation proceeds,');
   Z('various attributes of its orbit are plotted in a box to the side of the screen');
   Z('for each completed orbit. This is taken to be when it reaches a maximum');
   Z('distance from the centre of mass, or in other words, starts to fall back.');
   Z('No attempt is made to identify other orbital centres, such as the planet');
   Z('or the Trojan points of its orbit or whatever other stabilities exist, nor');
   Z('more baroque orbits such as about both the sun and the planet.');
   Z('   The attributes plotted (radius, angle of apastron, etc) can be selected');
   Z('or unselected by pressing the key for the first letter of the name, thus the');
   Z('plot of the successive values for the Apastron may be suppressed by pressing');
   Z('the A key, and so on. The plots appear as separate dots for each value, but if');
   Z('you prefer Joined-up plots, pressing the J key will switch states.');
   Z('   The S key switches between Stepwise and continual execution; pressing some');
   Z('other key (e.g. Return) then advances the calculation one step each time.');
   Z('   Everything depends on the initial state, and a distressingly long list of');
   Z('parameters only begins to explore the opportunities, as follows:');
   Z('');
   Z('   YearLength: days in the planet''s year, e.g. 1000');
   Z('   PlanetMass: as a fraction of the sun''s mass, e.g. 0.0125');
   Z('   AsteroidR:  as a fraction of the planet''s distance from the C of M');
   Z('   AsteroidA:  angle with the x-axis (in degrees)');
   Z('   AsteroidRV: radial velocity as a factor of the circular orbit velocity');
   Z('   AsteroidCV: circular orbit velocity factor');
   Z('   Style:      E = Euler, M = Euler 2''nd, R = Runge-Kutta.');
   Z('   SunMotion:  W = wobble about C of M, F = (improperly) fixed at C of M.');
   Z('');
   Z('   You need not supply all of these, but as they are not named, there can be');
   Z('no gap in the list. Thus, if you supply the third parameter then you must');
   Z('also supply the first and second, but need not supply those past the third.');
   Z('If no parameters are offered, the default is equivalent to');
   Z('');
   Z('   WANDER 1000 0.15  0.5 0  0.0 -0.75  R W');
   Z('');
   Z('   This will use the classic fourth-order Runge-Kutta scheme for numerical');
   Z('integration of a differential equation for the case where a massive planet');
   Z('(the Earth''s mass is 1/300000 of the mass of the Sun) orbits in a fixed');
   Z('circle having a thousand one-day steps, and a (massless) asteroid starts off');
   Z('on the x-axis halfway between the planet and the centre of mass with an');
   Z('initial velocity straight down of three quarters of the speed a circular orbit');
   Z('of that radius would require in the absence of planetary perturbations.');
   Z('');
   Z('   Don''t forget the leading 0 in 0.15 that turbo pascal demands...');
   Z('   ESC to quit.');
  END;

 Procedure Reject(Gripe: anystring);
  BEGIN
   Writeln;
   Writeln('Unsavoury: ',Gripe);
   Writeln;
   Grunt(3);
   Halt;
  END;
 Const Usage: array[1..8] of string = (
  'Steps in a planetary year',
  'Planetary mass ratio',
  'Asteroid''s initial radial distance',
  'Asteroid''s initial angular position',
  'Asteroid''s initial outwards velocity',
  'Asteroid''s initial orbital velocity',
  'Numerical Integration Method',
  'Fixed or Wobbling Sun position');
 Procedure Scrog(i: integer; var v: grist);
  var hic: integer;
  BEGIN
   hic:=-666;
   if paramstr(i) <> '' then Val(Paramstr(i),v,hic);
   if hic <> 0 then Reject(paramstr(i) + ' for ' + usage[i]);
  END;
 Function ScrogT(i: integer; var n: integer; List: string): boolean;
  var c: char;
  var stupid: string[1];
  BEGIN
   if paramstr(i) = '' then scrogt:=false
    else
     begin
      stupid:=copy(Paramstr(i),1,1);
      c:=char(hi(integer(stupid)));
      n:=Pos(upcase(c),List);
      if n <= 0 then Reject(paramstr(i) + ' not one of ' + list + ' for ' + usage[i]);
      scrogt:=true;
     end;
  END;


{$F+} Function HeapFull(Size: word): integer; {$F-}
       Begin HeapFull:=1; End; {Sez "If full, return a null pointer" to GetMem.}
{Damnit, Turbo pascal's pointer using procedures don't check for null pointers!}
 var i: Integer;
 BEGIN {The main programme at last.}
  HeapError:=@HeapFull;                {Memory shortages?}
  AsItWas:=LastMode;
  Writeln('                       A Wanderer in an unstable orbit.');
  Style:=3;
  Wobble:=true;
  Year:=1000;
  SM:=1; PM:=0.15;
  AR:=0.5; AA:=0;
  ARV:=0; ACV:=-0.75;
  if paramcount > 0 then
   begin
    if paramstr(1) = '?' then begin Grunt(1); exit; end;
    Scrog(1,year); year:=round(year); {Whole steps only!}
    Scrog(2,PM);
    Scrog(3,AR);
    Scrog(4,AA);
    Scrog(5,ARV);
    Scrog(6,ACV);
    if ScrogT(7,Style,'EMR') then style:=max(1,min(style,3));
    if ScrogT(8,i,'FW') then Wobble:=i = 2;
   end;
  AA:=Pi*AA/180;
  PrepareTheCanvas;
  WanderAlong;
  TextMode(AsItWas);
 END.

```



### Relativistic orbits

Another magazine article described a calculation for orbits twisted by the gravity around black holes. Only a two-body problem, and only just, because the orbiting body's mass is ignored.

```Pascal

{$N+ Crunch only the better sort of numbers, thanks.}
Program Swirl; Uses Graph, Crt;
{   Calculates and displays orbits strongly twisted by gravity, as described
 in the article by J. Gallmeier et al, in Sky & Telescope, October 1995, which
 included the source code of a programme written in basic, so some changes here.
    An actual pulsar in orbit about another compact body has parameters
 EC = .61713 and SG = .000025, resulting in a precession of .0037 degrees/orbit.
    More fierce relativistic effects involve close passages by a black hole.
 A dark disc appears corresponding to the event horizon diameter for a
 non-rotating black hole (the screen is normally 'black' so the black hole is
 not depicted as black) and a circle at twice its diameter is drawn; particles
 venturing within that bound will not repeat their orbit as they are either on
 the way down the drain, or else on a hyperbolic flypast.
    This calculation is for orbits that continue indefinitely, and the authors
 have used the classic Runge-Kutta fourth-order method to numerically integrate
 the relativistic equations twisting the axes of the orbit.}

{Perpetrated by R.N.McLean (whom God preserve), Victoria University, Nov. VMM.}

 Type Grist = {$IFOPT N+} extended {$ELSE} real {$ENDIF};
 const esc = #27;
 var colour,lastcolour,xmax,ymax,txtheight: integer;
 var stretch: grist;
 Type AnyString = string[80];

 FUNCTION Trim(S : anystring) : anystring;
 var L1,L2 : integer;
 BEGIN
   L1 := 1;
   WHILE (L1 <= LENGTH(S)) AND (S[L1] = ' ') DO INC(L1);
   L2 := LENGTH(S);
   WHILE (S[L2] = ' ') AND (L2 > L1) DO DEC(L2);
   IF L2 >= L1 THEN Trim := COPY(S,L1,L2 - L1 + 1) ELSE Trim := '';
 END; {Of Trim.}

 FUNCTION Ifmt(Digits : longint) : anystring;
  var  S : anystring;
  BEGIN
   STR(Digits,S);
   Ifmt := Trim(S);
  END; { Ifmt }

 FUNCTION Ffmt(Digits: grist; Width,Decimals: integer): anystring;
  var
   S : anystring;
   L : integer; { a finger }
  BEGIN
   if digits = 0 then begin Ffmt:='0'; exit; end; {Mumble.}
   IF Abs(Digits) < 0.0000001 THEN STR(Digits,S)
    ELSE STR(Digits:Width:Decimals,S);
   s:=trim(s);
   if copy(s,1,2) = '0.' then s:=copy(s,2,Length(s) - 1);
   if copy(s,1,3) = '-0.' then s:=concat('-',copy(s,3,Length(s) - 2));
   l:=Length(s);
   if pos('.',s) > 0 then while (l > 0) and (s[l] = '0') do l:=l - 1;
   if s[l] = '.' then l:=l - 1;         {No non-zero fractional part!}
   S := COPY(S,1,L);                    {Drop trailing boredom.}
   l:=Pos('.',S);                       {Avoid sequences such as 3.1415.}
   if l > 0 then s[l]:=chr(249);        {By using decimal points, please.}
   Ffmt := S;
  END; { Ffmt }

 Procedure Rogbiv;
  BEGIN;
   SetPalette( 0,Black);
   SetPalette( 1,DarkGray);  {Looks navy blue to me.}
   SetPalette( 2,Blue);
   SetPalette( 3,LightBlue);
   SetPalette( 4,LightCyan);
   SetPalette( 5,Cyan);
   SetPalette( 6,LightGreen);
   SetPalette( 7,Green);
   SetPalette( 8,LightGray); {Looks light brown to me.}
   SetPalette( 9,Yellow);
   SetPalette(10,Brown);     {Looks yellow to me.}
   SetPalette(11,Red);
   SetPalette(12,LightRed);
   SetPalette(13,Magenta);
   SetPalette(14,LightMagenta);
   SetPalette(15,White);
  END; {Of Rogbiv.}

 Procedure PrepareTheCanvas;
  Var mode: integer;
  Var SavedResultCode: integer;
  Var ax,ay: word;
  Function BGIFound(Here: Anystring): boolean;
   Var driver: integer;
   BEGIN
    Driver:=Detect;
    InitGraph(driver,mode,Here);
    SavedResultCode:=GraphResult;      {Mumble!}
    BGIFound:=SavedResultCode = 0;     {Missing file gives -3.}
   END; {Of BGIFound.}
  Var aplace: anystring;
  Var Palette: paletteType;
  BEGIN
   Mode:=0;
   if not BGIFound('') then
    if not BGIFound('C:\TP7\BGI') then
     if not BGIFound('D:\TP4') then
      begin
       WriteLn;
       WriteLn('Trouble with Borland''s Graphic Interface!');
       WriteLn('Message: ',GraphErrorMsg(SavedResultCode));
       WriteLn('I''ve tried "", "C:\TP4", and "D:\TP4"...');
       Repeat
        Write('Another place to look? (e.g. a:\bgiset)');
        ReadLn(Aplace);         {As opposed to Read. weird.}
        If Aplace = '' then
         begin
          writeln;
          write('No .bgi, no piccy.');
          halt;
         end;
       until BGIFound(Aplace);
      end;
   SetViewPort(0,0,GetMaxX,GetMaxY,True); {Clipping on, thanks. No apparent slowdown.}
   SetGraphMode(Mode);
   GetPalette(Palette);
   LastColour := Palette.Size - 1; {Colours are 0:15, 16 thereof.}
   if LastColour <= 0 then LastColour:=1;
   if LastColour = 15 then Rogbiv;
   xmax:=GetMaxX; ymax:=GetMaxY;
   txtheight:=TextHeight('I');
   GetAspectRatio(ax,ay); stretch:=ay/ax; {I hope this handles the screen's physical dimensions}
  END;                                    {as well as the number of dots in the x and y directions.}

 Procedure Splot(x,y:integer; bumf: anystring);
  var l: integer;
  BEGIN
   l:=TextWidth(Bumf);
   if x + l > xmax then x:=xmax - l;
   SetFillStyle(EmptyFill,Black); Bar(x,y,x + l,y + txtheight - 1);
   OutTextXY(x,y,bumf);
  END;

 Var nstep: longint;
 Procedure SplotStep;
  BEGIN
   Splot(xmax,0 + TxtHeight,'Stepcount: ' + ifmt(nstep));
  END;


 Var EC,SG,SS,SA: grist;
 Var n: longint;
 Procedure PrecessRelativistically;
  var
   SX,SY,
   C0,C2,RH,
   Q,SN,QN,DN,HN,DP,SP,
   K1,K2,K3,K4,L1,L2,L3,L4: grist;
  var xc,yc,px,py: integer;
  Procedure Fullturn;
   BEGIN
    Line(xc,yc, round(PX),round(PY));
    n:=n + 1;
    Splot(xmax,0,'Completed orbits ' + ifmt(n));
    SplotStep;
    if n = 1 then            {First time round?}
     begin                   {Yep. DP and DN straddle zero. Find S where D = 0.}
      SA:=(SP - DP*(SN - SP)/(DN - DP))*180/Pi - 360;
      Splot(xmax,ymax - txtheight,'Precession per orbit ' + ffmt(SA,15,5) + 'ø');
     end;                    {y:x=0 given (x1,y1) and (x2,y2):  y = y1 - x1*(y2 - y1)/(x2 - x1)}
    colour:=colour + 1;
    if colour > Lastcolour then colour:=3;
    setcolor(colour);
   END;
  Var ch: char;
  BEGIN
   Splot(0,0,'Eccentricity ' + ffmt(EC,9,6));
   Splot(0,ymax - txtheight,'Relativistic Factor ' + ffmt(SG,9,6));
   Splot(xmax,0 + 2*txtheight,'StepSize ' + ffmt(SS,8,3));
   xc:=xmax div 2; yc:=ymax div 2;
   n:=0; nstep:=0;
   SS:=0.0009*(SS - 0.9)*(1 - 0.9*exp(10*EC - 9)); {Step size stuff...}
   SY:=yc/(1 + EC);  SX:=SY*stretch;
   C0:=(1 - SG*(3 + EC*EC)/(6 + 2*EC))/(1 - EC*EC);
   RH:=SG*(1 - EC*EC)/(3 + EC);
   C2:=1.5*RH;
   SA:=0;
   Colour:=1; SetColor(Colour);    Circle(xc,yc,round(SX*RH));
   SetFillStyle(SolidFill,Colour); FloodFill(xc,yc,Colour);
                                   Circle(xc,yc,round(SX*RH*2));
   Colour:=2; SetColor(Colour); Line(0,yc, xmax,yc); Line(xc,0, xc,ymax);
   Colour:=3; SetColor(Colour);
   SN:=0; QN:=1/(1 + EC); DN:=0;
   repeat
    DP:=DN; SP:=SN;
    HN:=SS*QN*QN;
    Q:=QN;        K1:=(C0 - Q + C2*Q*Q)*HN; L1:=HN*DN;
    Q:=QN + L1/2; K2:=(C0 - Q + C2*Q*Q)*HN; L2:=HN*(DN + K1/2);
    Q:=QN + L2/2; K3:=(C0 - Q + C2*Q*Q)*HN; L3:=HN*(DN + K2/2);
    Q:=QN + L3;   K4:=(C0 - Q + C2*Q*Q)*HN; L4:=HN*(DN + K3);
    QN:=QN + (L1 + (L2 + L3)*2 + L4)/6;
    DN:=DN + (K1 + (K2 + K3)*2 + K4)/6;
    SN:=SN + HN;
    px:=xc + Round(SX*cos(SN)/QN); py:=yc - Round(SY*sin(SN)/QN);
    PutPixel(px,py,Colour);
    nstep:=nstep + 1;
    if (DN > 0) and (DP < 0) then fullturn
     else if nstep mod 250 = 0 then splotstep;
   until keypressed;
   if readkey <> esc then repeat until keypressed;
  END; {Of PrecessRelativistically.}

 Procedure Grunt;
  BEGIN
   WriteLn('Draws orbits under relativistic conditions.');
   WriteLn('Activate with two parameters:');
   WriteLn('   Orbital Eccentricity (0 to 0ù9),');
   WriteLn('   Relativistic factor  (0 to 0ù999)');
   WriteLn('An optional third parameter is the step size (1 to 10, e.g. 5).');
   WriteLn('(If no parameters are supplied, an example run results)');
   WriteLn('Eg.  Swirl 0.5 0.5 5');
   WriteLn('Don''t forget the leading zeroes that turbo pascal demands...');
   WriteLn('ESC to quit.');
  END;

 Procedure Reject(Gripe: anystring);
  BEGIN
   Writeln;
   Writeln('Unsavoury: ',Gripe);
   Writeln;
   Grunt;
   Halt;
  END;

 Var AsItWas: word;
 Var i: integer;
 Var Hic: array[1..3] of integer;
 BEGIN
  AsItWas:=LastMode;
  Writeln('                       Precession of Elliptical Orbits.');
  EC:=0.8; SG:=0.3826057; SS:=5;        {These values result in 72 degrees/orbit...}
  if paramcount > 0 then
   begin
    if paramstr(1) = '?' then begin Grunt; exit; end;
    Val(ParamStr(1),EC,hic[1]);
    Val(ParamStr(2),SG,hic[2]);
    hic[3]:=0; if paramcount >= 3 then Val(paramstr(3),SS,Hic[3]);
    for i:=1 to 3 do if hic[i] > 0 then Reject('Parameter ' + ifmt(i) + ': ' + paramstr(i));
   end;
  if (ec < 0) or (ec >= 1) then Reject('an eccentricity of ' + ffmt(ec,15,5));
  if (sg < 0) or (sg >= 1) then Reject('a relativistic factor of ' + ffmt(sg,15,5));
  if ss <= 0 then Reject('a step size of ' + ffmt(ss,15,5));
  PrepareTheCanvas;
  PrecessRelativistically;
  TextMode(AsItWas);
  WriteLn('Orbital eccentricity:',EC:15:5);
  WriteLn('Relativistic factor :',SG:15:5);
  WriteLn('Precession per orbit:',SA:15:5);
  WriteLn('Orbits completed: ',n);
  WriteLn('Calculation steps:',nstep);
 END.

```



## Perl

Cycle through one Neptunian year.

```perl
use strict;
use warnings;

use constant PI         => 3.141592653589793;
use constant SOLAR_MASS => (4 * PI * PI);
use constant YEAR       => 365.24;

sub solar_offset {
    my($vxs, $vys, $vzs, $mass) = @_;
    my($px, $py, $pz);
    for (0..@$mass-1) {
        $px += @$vxs[$_] * @$mass[$_];
        $py += @$vys[$_] * @$mass[$_];
        $pz += @$vzs[$_] * @$mass[$_];
    }
    (@$vxs[0], @$vys[0], @$vzs[0]) = ((-$px/SOLAR_MASS), (-$py/SOLAR_MASS), (-$pz/SOLAR_MASS));
}

sub orbit {
    my ($dt, $xs, $ys, $zs, $vxs, $vys, $vzs, $mass) = @_;
    for (0..@$mass-1) {
        for (my $j = $_ + 1; $j < @$mass; $j++) {
            my($dx, $dy, $dz) = ((@$xs[$_] - @$xs[$j]), (@$ys[$_] - @$ys[$j]), (@$zs[$_] - @$zs[$j]));
            my $mag = $dt / ($dx**2 + $dy**2 + $dz**2)**(3/2);
            my($mm, $mm2) = ((@$mass[$_] * $mag), (@$mass[$j] * $mag));
            @$vxs[$_] -= $dx * $mm2;
            @$vxs[$j] += $dx * $mm;
            @$vys[$_] -= $dy * $mm2;
            @$vys[$j] += $dy * $mm;
            @$vzs[$_] -= $dz * $mm2;
            @$vzs[$j] += $dz * $mm;
        }
        @$xs[$_] += $dt * @$vxs[$_];
        @$ys[$_] += $dt * @$vys[$_];
        @$zs[$_] += $dt * @$vzs[$_];
    }
}

sub display {
    my($t,$xs,$ys) = @_;
    printf '%6d', $t;
    printf '%7.2f' x 2, @$xs[$_],@$ys[$_] for 1..4;
    print "\n";
}

my @ns =                         <Sun   Jupiter          Saturn           Uranus           Neptune       >;
my @xs =                         <0     4.84143144e+00   8.34336671e+00   1.28943695e+01   1.53796971e+01>;
my @ys =                         <0    -1.16032004e+00   4.12479856e+00  -1.51111514e+01  -2.59193146e+01>;
my @zs =                         <0    -1.03622044e-01  -4.03523417e-01  -2.23307578e-01   1.79258772e-01>;
my @vxs = map {$_ * YEAR}        <0     1.66007664e-03  -2.76742510e-03   2.96460137e-03   2.68067772e-03>;
my @vys = map {$_ * YEAR}        <0     7.69901118e-03   4.99852801e-03   2.37847173e-03   1.62824170e-03>;
my @vzs = map {$_ * YEAR}        <0    -6.90460016e-05   2.30417297e-05  -2.96589568e-05  -9.51592254e-05>;
my @mass = map {$_ * SOLAR_MASS} <1     9.54791938e-04   2.85885980e-04   4.36624404e-05   5.15138902e-05>;
solar_offset(\@vxs, \@vys, \@vzs, \@mass);

printf '     t' . '%14s'x4 . "\n", @ns[1..4];
display(0, \@xs, \@ys);
my $steps = 16567;
for my $t (1 .. $steps) {
    orbit(0.01, \@xs, \@ys, \@zs, \@vxs, \@vys, \@vzs, \@mass);
    display($t,\@xs, \@ys) unless $t % 1000;
}
display($steps, \@xs, \@ys);
```

{{out}}

```txt
             Jupiter        Saturn        Uranus       Neptune
     t      x      y      x      y      x      y      x      y
     0   4.84  -1.16   8.34   4.12  12.89 -15.11  15.38 -25.92
  1000   1.42  -5.00  -8.75   3.29  19.78  -3.52  23.83 -18.26
  2000  -3.44  -4.20   0.84 -10.06  17.37   9.73  28.84  -7.96
  3000  -5.46   0.16   7.88   4.86   6.60  18.23  29.65   3.50
  4000  -2.98   4.35  -9.06   2.44  -7.48  17.22  26.11  14.44
  5000   2.11   4.56   1.68  -9.94 -17.23   6.52  18.74  23.26
  6000   4.95   0.15   7.30   5.63 -16.69  -8.04   8.63  28.68
  7000   2.57  -4.46  -9.31   1.56  -6.34 -17.96  -2.73  29.94
  8000  -2.40  -4.82   2.49  -9.78   7.59 -18.13 -13.71  26.90
  9000  -5.37  -1.03   6.71   6.26  17.81  -9.20 -22.74  20.04
 10000  -3.94   3.59  -9.45   0.70  19.61   4.12 -28.58  10.36
 11000   0.89   5.00   3.29  -9.51  12.12  15.46 -30.46  -0.75
 12000   4.73   1.45   6.00   6.91  -1.36  18.98 -28.13 -11.76
 13000   3.55  -3.63  -9.53  -0.20 -13.97  12.16 -21.93 -21.16
 14000  -1.24  -5.19   4.06  -9.21 -18.34  -1.74 -12.70 -27.63
 15000  -5.02  -2.17   5.29   7.42 -11.77 -14.64  -1.71 -30.27
 16000  -4.70   2.63  -9.52  -1.06   1.54 -19.34   9.51 -28.67
 16567   3.45  -3.74  -3.24  -9.52   9.24 -17.42  15.38 -25.92
```



## Perl 6


We'll try to simulate the Sun+Earth+Moon system, with plausible astronomical values.

We use a 18-dimension vector <math>ABC</math>.  The first nine dimensions are the positions of the three bodies.  The other nine are the velocities.  This allows us to write the dynamics as a first-temporal derivative equation, since

<math>\frac{d\mathrm{position}}{dt} = \mathrm{speed}</math>

and thus

<math>\frac{dABC_{1..9}}{dt} = ABC_{10..18}</math>

To keep things compact, we'll only display the first 20 lines of output.


```perl6
# Simple Vector implementation
multi infix:<+>(@a, @b) { @a Z+ @b }
multi infix:<->(@a, @b) { @a Z- @b }
multi infix:<*>($r, @a) { $r X* @a }
multi infix:</>(@a, $r) { @a X/ $r }
sub norm { sqrt [+] @_ X** 2 }
 
# Runge-Kutta stuff
sub runge-kutta(&yp) {
    return -> \t, \y, \δt {
        my $a = δt * yp( t, y );
        my $b = δt * yp( t + δt/2, y + $a/2 );
        my $c = δt * yp( t + δt/2, y + $b/2 );
        my $d = δt * yp( t + δt, y + $c );
        ($a + 2*($b + $c) + $d) / 6;
    }
}
 
# gravitational constant
constant G = 6.674e-11;
# astronomical unit
constant au = 150e9;
 
# time constants in seconds
constant year = 365.25*24*60*60;
constant month = 21*24*60*60;
 
# masses in kg
constant $ma = 2e30;     # Sun
constant $mb = 6e24;     # Earth
constant $mc = 7.34e22;  # Moon

my &dABC = runge-kutta my &f = sub ( $t, @ABC ) {
    my @a = @ABC[0..2];
    my @b = @ABC[3..5];
    my @c = @ABC[6..8];
 
    my $ab = norm(@a - @b); 
    my $ac = norm(@a - @c);
    my $bc = norm(@b - @c);
 
    return [
        flat
        @ABC[@(9..17)],
        map G * *,
        $mb/$ab**3 * (@b - @a) + $mc/$ac**3 * (@c - @a),
        $ma/$ab**3 * (@a - @b) + $mc/$bc**3 * (@c - @b),
        $ma/$ac**3 * (@a - @c) + $mb/$bc**3 * (@b - @c);
    ];
}
 
loop (
    my ($t, @ABC) = 0,
        0, 0, 0,                                 # Sun position
        au, 0, 0,                                # Earth position
        0.998*au, 0, 0,                          # Moon position
        0, 0, 0,                                 # Sun speed
        0, 2*pi*au/year, 0,                      # Earth speed
        0, 2*pi*(au/year + 0.002*au/month), 0    # Moon speed
    ;
    $t < .2;
    ($t, @ABC) »+=« (.01, dABC($t, @ABC, .01))
) {
    printf "t = %.02f : %s\n", $t, @ABC.fmt("%+.3e");
}
```

{{out}}

```txt
t = 0.00 : +0.000e+00 +0.000e+00 +0.000e+00 +1.500e+11 +0.000e+00 +0.000e+00 +1.497e+11 +0.000e+00 +0.000e+00 +0.000e+00 +0.000e+00 +0.000e+00 +0.000e+00 +2.987e+04 +0.000e+00 +0.000e+00 +3.090e+04 +0.000e+00
t = 0.01 : +9.008e-13 +5.981e-22 +0.000e+00 +1.500e+11 +2.987e+02 +0.000e+00 +1.497e+11 +3.090e+02 +0.000e+00 +1.802e-10 +1.794e-19 +0.000e+00 -5.987e-05 +2.987e+04 +0.000e+00 -1.507e-05 +3.090e+04 +0.000e+00
t = 0.02 : +3.603e-12 +4.785e-21 +0.000e+00 +1.500e+11 +5.973e+02 +0.000e+00 +1.497e+11 +6.181e+02 +0.000e+00 +3.603e-10 +7.177e-19 +0.000e+00 -1.197e-04 +2.987e+04 +0.000e+00 -3.014e-05 +3.090e+04 +0.000e+00
t = 0.03 : +8.107e-12 +1.615e-20 +0.000e+00 +1.500e+11 +8.960e+02 +0.000e+00 +1.497e+11 +9.271e+02 +0.000e+00 +5.405e-10 +1.615e-18 +0.000e+00 -1.796e-04 +2.987e+04 +0.000e+00 -4.521e-05 +3.090e+04 +0.000e+00
t = 0.04 : +1.441e-11 +3.828e-20 +0.000e+00 +1.500e+11 +1.195e+03 +0.000e+00 +1.497e+11 +1.236e+03 +0.000e+00 +7.206e-10 +2.871e-18 +0.000e+00 -2.395e-04 +2.987e+04 +0.000e+00 -6.028e-05 +3.090e+04 +0.000e+00
t = 0.05 : +2.252e-11 +7.476e-20 +0.000e+00 +1.500e+11 +1.493e+03 +0.000e+00 +1.497e+11 +1.545e+03 +0.000e+00 +9.008e-10 +4.486e-18 +0.000e+00 -2.993e-04 +2.987e+04 +0.000e+00 -7.535e-05 +3.090e+04 +0.000e+00
t = 0.06 : +3.243e-11 +1.292e-19 +0.000e+00 +1.500e+11 +1.792e+03 +0.000e+00 +1.497e+11 +1.854e+03 +0.000e+00 +1.081e-09 +6.460e-18 +0.000e+00 -3.592e-04 +2.987e+04 +0.000e+00 -9.041e-05 +3.090e+04 +0.000e+00
t = 0.07 : +4.414e-11 +2.051e-19 +0.000e+00 +1.500e+11 +2.091e+03 +0.000e+00 +1.497e+11 +2.163e+03 +0.000e+00 +1.261e-09 +8.792e-18 +0.000e+00 -4.191e-04 +2.987e+04 +0.000e+00 -1.055e-04 +3.090e+04 +0.000e+00
t = 0.08 : +5.765e-11 +3.062e-19 +0.000e+00 +1.500e+11 +2.389e+03 +0.000e+00 +1.497e+11 +2.472e+03 +0.000e+00 +1.441e-09 +1.148e-17 +0.000e+00 -4.789e-04 +2.987e+04 +0.000e+00 -1.206e-04 +3.090e+04 +0.000e+00
t = 0.09 : +7.296e-11 +4.360e-19 +0.000e+00 +1.500e+11 +2.688e+03 +0.000e+00 +1.497e+11 +2.781e+03 +0.000e+00 +1.621e-09 +1.453e-17 +0.000e+00 -5.388e-04 +2.987e+04 +0.000e+00 -1.356e-04 +3.090e+04 +0.000e+00
t = 0.10 : +9.008e-11 +5.981e-19 +0.000e+00 +1.500e+11 +2.987e+03 +0.000e+00 +1.497e+11 +3.090e+03 +0.000e+00 +1.802e-09 +1.794e-17 +0.000e+00 -5.987e-04 +2.987e+04 +0.000e+00 -1.507e-04 +3.090e+04 +0.000e+00
t = 0.11 : +1.090e-10 +7.961e-19 +0.000e+00 +1.500e+11 +3.285e+03 +0.000e+00 +1.497e+11 +3.399e+03 +0.000e+00 +1.982e-09 +2.171e-17 +0.000e+00 -6.586e-04 +2.987e+04 +0.000e+00 -1.658e-04 +3.090e+04 +0.000e+00
t = 0.12 : +1.297e-10 +1.034e-18 +0.000e+00 +1.500e+11 +3.584e+03 +0.000e+00 +1.497e+11 +3.709e+03 +0.000e+00 +2.162e-09 +2.584e-17 +0.000e+00 -7.184e-04 +2.987e+04 +0.000e+00 -1.808e-04 +3.090e+04 +0.000e+00
t = 0.13 : +1.522e-10 +1.314e-18 +0.000e+00 +1.500e+11 +3.882e+03 +0.000e+00 +1.497e+11 +4.018e+03 +0.000e+00 +2.342e-09 +3.032e-17 +0.000e+00 -7.783e-04 +2.987e+04 +0.000e+00 -1.959e-04 +3.090e+04 +0.000e+00
t = 0.14 : +1.766e-10 +1.641e-18 +0.000e+00 +1.500e+11 +4.181e+03 +0.000e+00 +1.497e+11 +4.327e+03 +0.000e+00 +2.522e-09 +3.517e-17 +0.000e+00 -8.382e-04 +2.987e+04 +0.000e+00 -2.110e-04 +3.090e+04 +0.000e+00
t = 0.15 : +2.027e-10 +2.019e-18 +0.000e+00 +1.500e+11 +4.480e+03 +0.000e+00 +1.497e+11 +4.636e+03 +0.000e+00 +2.702e-09 +4.037e-17 +0.000e+00 -8.980e-04 +2.987e+04 +0.000e+00 -2.260e-04 +3.090e+04 +0.000e+00
t = 0.16 : +2.306e-10 +2.450e-18 +0.000e+00 +1.500e+11 +4.778e+03 +0.000e+00 +1.497e+11 +4.945e+03 +0.000e+00 +2.883e-09 +4.593e-17 +0.000e+00 -9.579e-04 +2.987e+04 +0.000e+00 -2.411e-04 +3.090e+04 +0.000e+00
t = 0.17 : +2.603e-10 +2.938e-18 +0.000e+00 +1.500e+11 +5.077e+03 +0.000e+00 +1.497e+11 +5.254e+03 +0.000e+00 +3.063e-09 +5.186e-17 +0.000e+00 -1.018e-03 +2.987e+04 +0.000e+00 -2.562e-04 +3.090e+04 +0.000e+00
t = 0.18 : +2.919e-10 +3.488e-18 +0.000e+00 +1.500e+11 +5.376e+03 +0.000e+00 +1.497e+11 +5.563e+03 +0.000e+00 +3.243e-09 +5.814e-17 +0.000e+00 -1.078e-03 +2.987e+04 +0.000e+00 -2.712e-04 +3.090e+04 +0.000e+00
t = 0.19 : +3.252e-10 +4.102e-18 +0.000e+00 +1.500e+11 +5.674e+03 +0.000e+00 +1.497e+11 +5.872e+03 +0.000e+00 +3.423e-09 +6.477e-17 +0.000e+00 -1.138e-03 +2.987e+04 +0.000e+00 -2.863e-04 +3.090e+04 +0.000e+00
```



## Phix

{{trans|Kotlin}}

```Phix
sequence origin = {0,0,0}
 
constant nbody = """
0.01 3 20
1
0 0 0
0.01 0 0
0.1
1 1 0
0 0 0.02
0.001
0 1 1
0.01 -0.01 -0.01
"""

sequence lines = split(nbody,"\n") 
atom {{grav_constant,bodies,timeSteps}} = scanf(lines[1],"%f %f %f")
sequence {masses, positions, velocities, accelerations} @= repeat(0,bodies)
for i=1 to bodies do
    {{masses[i]}} = scanf(lines[i*3-1],"%f")        
    {positions[i]} = scanf(lines[i*3],"%f %f %f")
    {velocities[i]} = scanf(lines[i*3+1],"%f %f %f")        
end for
printf(1,"Body   :      x          y          z    |")
printf(1,"     vx         vy         vz\n")

function vmod(sequence a)
    return sqrt(sum(sq_mul(a,a)))
end function

procedure computeAccelerations()
    for i=1 to bodies do
        sequence ai = origin
        for j=1 to bodies do
            if i!=j then
                sequence t = sq_sub(positions[j],positions[i])
                atom temp = grav_constant*masses[j]/power(vmod(t),3)
                ai = sq_add(ai,sq_mul(t,temp))
            end if
        end for
        accelerations[i] = ai
    end for
end procedure
 
procedure computePositions()
    positions = sq_add(positions,sq_add(velocities,sq_mul(accelerations,0.5)))
end procedure

procedure computeVelocities()
    velocities = sq_add(velocities,accelerations)
end procedure

procedure resolveCollisions()
    for i=1 to bodies do
        for j=i+1 to bodies do
            if positions[i]=positions[j] then
                {velocities[i],velocities[j]} = {velocities[j],velocities[i]}
            end if
        end for
    end for
end procedure

procedure simulate()
    computeAccelerations()
    computePositions()
    computeVelocities()
    resolveCollisions()
end procedure
 
procedure printResults()
    string fmt = "Body %d : %9.6f  %9.6f  %9.6f | %9.6f  %9.6f  %9.6f\n"
    for i=1 to bodies do
        printf(1,fmt,{i}&positions[i]&velocities[i])
    end for
end procedure
 
for i=1 to timeSteps do
    printf(1,"\nCycle %d\n",i)
    simulate()
    printResults()
end for
```

Output same as Python (and C and C# and D and Kotlin and Go)


## Python


```python
import math

class Vector:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y, self.z + other.z)

    def __sub__(self, other):
        return Vector(self.x - other.x, self.y - other.y, self.z - other.z)

    def __mul__(self, other):
        return Vector(self.x * other, self.y * other, self.z * other)

    def __div__(self, other):
        return Vector(self.x / other, self.y / other, self.z / other)

    def __eq__(self, other):
        if isinstance(other, Vector):
            return self.x == other.x and self.y == other.y and self.z == other.z
        return False

    def __ne__(self, other):
        return not self.__eq__(other)

    def __str__(self):
        return '({x}, {y}, {z})'.format(x=self.x, y=self.y, z=self.z)

    def abs(self):
        return math.sqrt(self.x*self.x + self.y*self.y + self.z*self.z)

origin = Vector(0, 0, 0)

class NBody:
    def __init__(self, fileName):
        with open(fileName, "r") as fh:
            lines = fh.readlines()
            gbt = lines[0].split()
            self.gc = float(gbt[0])
            self.bodies = int(gbt[1])
            self.timeSteps = int(gbt[2])
            self.masses = [0.0 for i in range(self.bodies)]
            self.positions = [origin for i in range(self.bodies)]
            self.velocities = [origin for i in range(self.bodies)]
            self.accelerations = [origin for i in range(self.bodies)]
            for i in range(self.bodies):
                self.masses[i] = float(lines[i*3 + 1])
                self.positions[i] = self.__decompose(lines[i*3 + 2])
                self.velocities[i] = self.__decompose(lines[i*3 + 3])

            print "Contents of", fileName
            for line in lines:
                print line.rstrip()
            print
            print "Body   :      x          y          z    |",
            print "     vx         vy         vz"

    def __decompose(self, line):
        xyz = line.split()
        x = float(xyz[0])
        y = float(xyz[1])
        z = float(xyz[2])
        return Vector(x, y, z)

    def __computeAccelerations(self):
        for i in xrange(self.bodies):
            self.accelerations[i] = origin
            for j in xrange(self.bodies):
                if i != j:
                    temp = self.gc * self.masses[j] / math.pow((self.positions[i] - self.positions[j]).abs(), 3)
                    self.accelerations[i] += (self.positions[j] - self.positions[i]) * temp
        return None

    def __computePositions(self):
        for i in xrange(self.bodies):
            self.positions[i] += self.velocities[i] + self.accelerations[i] * 0.5
        return None

    def __computeVelocities(self):
        for i in xrange(self.bodies):
            self.velocities[i] += self.accelerations[i]
        return None

    def __resolveCollisions(self):
        for i in xrange(self.bodies):
            for j in xrange(self.bodies):
                if self.positions[i] == self.positions[j]:
                    (self.velocities[i], self.velocities[j]) = (self.velocities[j], self.velocities[i])
        return None

    def simulate(self):
        self.__computeAccelerations()
        self.__computePositions()
        self.__computeVelocities()
        self.__resolveCollisions()
        return None

    def printResults(self):
        fmt = "Body %d : % 8.6f  % 8.6f  % 8.6f | % 8.6f  % 8.6f  % 8.6f"
        for i in xrange(self.bodies):
            print fmt % (i+1, self.positions[i].x, self.positions[i].y, self.positions[i].z, self.velocities[i].x, self.velocities[i].y, self.velocities[i].z)
        return None

nb = NBody("nbody.txt")
for i in xrange(nb.timeSteps):
    print "\nCycle %d" % (i + 1)
    nb.simulate()
    nb.printResults()
```

{{out}}

```txt
Contents of nbody.txt
0.01 3 20
1
0 0 0
0.01 0 0
0.1
1 1 0
0 0 0.02
0.001
0 1 1
0.01 -0.01 -0.01

Body   :      x          y          z    |      vx         vy         vz

Cycle 1
Body 1 :  0.010177   0.000179   0.000002 |  0.010354   0.000357   0.000004
Body 2 :  0.998230   0.998232   0.020002 | -0.003539  -0.003536   0.020004
Body 3 :  0.010177   0.988232   0.988055 |  0.010354  -0.013536  -0.013889

Cycle 2
Body 1 :  0.020709   0.000718   0.000011 |  0.010710   0.000721   0.000014
Body 2 :  0.992907   0.992896   0.039971 | -0.007109  -0.007138   0.019935
Body 3 :  0.020717   0.972888   0.972173 |  0.010727  -0.017153  -0.017876

Cycle 3
Body 1 :  0.031600   0.001625   0.000034 |  0.011072   0.001094   0.000033
Body 2 :  0.983985   0.983910   0.059834 | -0.010735  -0.010835   0.019790
Body 3 :  0.031643   0.953868   0.952235 |  0.011125  -0.020886  -0.021999

Cycle 4
Body 1 :  0.042858   0.002913   0.000081 |  0.011443   0.001481   0.000060
Body 2 :  0.971393   0.971163   0.079509 | -0.014448  -0.014659   0.019561
Body 3 :  0.042981   0.931039   0.928087 |  0.011552  -0.024772  -0.026299

Cycle 5
Body 1 :  0.054492   0.004595   0.000160 |  0.011826   0.001884   0.000097
Body 2 :  0.955030   0.954509   0.098909 | -0.018278  -0.018649   0.019238
Body 3 :  0.054766   0.904225   0.899522 |  0.012018  -0.028857  -0.030829

Cycle 6
Body 1 :  0.066517   0.006691   0.000281 |  0.012224   0.002308   0.000145
Body 2 :  0.934759   0.933760   0.117931 | -0.022265  -0.022849   0.018806
Body 3 :  0.067040   0.873197   0.866280 |  0.012530  -0.033199  -0.035655

Cycle 7
Body 1 :  0.078950   0.009225   0.000456 |  0.012642   0.002759   0.000206
Body 2 :  0.910400   0.908677   0.136456 | -0.026454  -0.027316   0.018244
Body 3 :  0.079856   0.837662   0.828023 |  0.013101  -0.037871  -0.040861

Cycle 8
Body 1 :  0.091815   0.012227   0.000702 |  0.013086   0.003245   0.000284
Body 2 :  0.881722   0.878958   0.154340 | -0.030902  -0.032122   0.017523
Body 3 :  0.093281   0.797239   0.784313 |  0.013749  -0.042975  -0.046559

Cycle 9
Body 1 :  0.105140   0.015737   0.001035 |  0.013564   0.003775   0.000383
Body 2 :  0.848429   0.844216   0.171401 | -0.035684  -0.037362   0.016600
Body 3 :  0.107405   0.751427   0.734579 |  0.014498  -0.048649  -0.052908

Cycle 10
Body 1 :  0.118964   0.019805   0.001481 |  0.014085   0.004362   0.000509
Body 2 :  0.810137   0.803953   0.187408 | -0.040900  -0.043166   0.015414
Body 3 :  0.122346   0.699554   0.678056 |  0.015384  -0.055097  -0.060138

Cycle 11
Body 1 :  0.133337   0.024498   0.002071 |  0.014662   0.005025   0.000672
Body 2 :  0.766343   0.757509   0.202050 | -0.046687  -0.049720   0.013868
Body 3 :  0.138268   0.640690   0.613685 |  0.016460  -0.062633  -0.068603

Cycle 12
Body 1 :  0.148327   0.029907   0.002851 |  0.015317   0.005792   0.000888
Body 2 :  0.716377   0.703998   0.214889 | -0.053246  -0.057302   0.011810
Body 3 :  0.155406   0.573482   0.539941 |  0.017816  -0.071782  -0.078886

Cycle 13
Body 1 :  0.164025   0.036157   0.003887 |  0.016079   0.006709   0.001184
Body 2 :  0.659310   0.642172   0.225282 | -0.060887  -0.066351   0.008976
Body 3 :  0.174112   0.495836   0.454475 |  0.019596  -0.083511  -0.092045

Cycle 14
Body 1 :  0.180564   0.043437   0.005286 |  0.017000   0.007852   0.001613
Body 2 :  0.593807   0.570186   0.232208 | -0.070119  -0.077621   0.004875
Body 3 :  0.194929   0.404136   0.353320 |  0.022038  -0.099890  -0.110265

Cycle 15
Body 1 :  0.198150   0.052049   0.007234 |  0.018171   0.009372   0.002283
Body 2 :  0.517817   0.485100   0.233878 | -0.081861  -0.092550  -0.001535
Body 3 :  0.218605   0.290860   0.228583 |  0.025314  -0.126661  -0.139210

Cycle 16
Body 1 :  0.217126   0.062542   0.010117 |  0.019781   0.011614   0.003484
Body 2 :  0.427899   0.381659   0.226654 | -0.097974  -0.114332  -0.012913
Body 3 :  0.244268   0.131956   0.057562 |  0.026013  -0.191148  -0.202831

Cycle 17
Body 1 :  0.238346   0.076539   0.015221 |  0.022658   0.016380   0.006723
Body 2 :  0.317489   0.248502   0.200967 | -0.122846  -0.151982  -0.038461
Body 3 :  0.075592  -0.559591  -0.487315 | -0.363366  -1.191945  -0.886924

Cycle 18
Body 1 :  0.263123   0.097523   0.026918 |  0.026898   0.025587   0.016672
Body 2 :  0.173428   0.050424   0.112716 | -0.165275  -0.244174  -0.138041
Body 3 : -0.286241  -1.745597  -1.369528 | -0.360299  -1.180066  -0.877501

Cycle 19
Body 1 :  0.270854   0.113045   0.061923 | -0.011436   0.005457   0.053339
Body 2 :  0.199821  -0.093105  -0.208666 |  0.218061  -0.042882  -0.504723
Body 3 : -0.646318  -2.924909  -2.246453 | -0.359856  -1.178559  -0.876350

Cycle 20
Body 1 :  0.258572   0.116046   0.112038 | -0.013129   0.000544   0.046890
Body 2 :  0.426346  -0.111425  -0.681150 |  0.234987   0.006241  -0.440245
Body 3 : -1.006089  -4.103186  -3.122591 | -0.359686  -1.177995  -0.875924
```



## Swift



```swift
import Foundation

public struct Vector {
  public var px = 0.0
  public var py = 0.0
  public var pz = 0.0

  public init(px: Double, py: Double, pz: Double) {
    (self.px, self.py, self.pz) = (px, py, pz)
  }

  public init?(array: [Double]) {
    guard array.count == 3 else {
      return nil
    }

    (self.px, self.py, self.pz) = (array[0], array[1], array[2])
  }

  public func mod() -> Double {
    (px * px + py * py + pz * pz).squareRoot()
  }

  static func + (lhs: Vector, rhs: Vector) -> Vector {
    return Vector(
      px: lhs.px + rhs.px,
      py: lhs.py + rhs.py,
      pz: lhs.pz + rhs.pz
    )
  }

  static func - (lhs: Vector, rhs: Vector) -> Vector {
    return Vector(
      px: lhs.px - rhs.px,
      py: lhs.py - rhs.py,
      pz: lhs.pz - rhs.pz
    )
  }

  static func * (lhs: Vector, rhs: Double) -> Vector {
    return Vector(
      px: lhs.px * rhs,
      py: lhs.py * rhs,
      pz: lhs.pz * rhs
    )
  }
}

extension Vector {
  public static let origin = Vector(px: 0, py: 0, pz: 0)
}

extension Vector: Equatable {
  public static func == (lhs: Vector, rhs: Vector) -> Bool {
    return lhs.px == rhs.px && lhs.py == rhs.py && lhs.pz == rhs.pz
  }
}

extension Vector: CustomStringConvertible {
  public var description: String {
    return String(format: "%.6f\t%.6f\t%.6f", px, py, pz)
  }
}

public class NBody {
  public let gravitationalConstant: Double
  public let numBodies: Int
  public let timeSteps: Int

  public private(set) var masses: [Double]
  public private(set) var positions: [Vector]
  public private(set) var velocities: [Vector]
  public private(set) var accelerations: [Vector]

  public init?(file: String) {
    guard let data = try? String(contentsOfFile: file) else {
      return nil
    }

    print("Input file:\n\(data)")

    let lines = data.components(separatedBy: "\n").map({ $0.components(separatedBy: " ") })

    let worldData = lines.first!

    guard worldData.count == 3,
          let gc = Double(worldData[0]),
          let bodies = Int(worldData[1]),
          let timeSteps = Int(worldData[2]) else {
      return nil
    }

    let defaultState = Array(repeating: Vector.origin, count: bodies)

    self.gravitationalConstant = gc
    self.numBodies = bodies
    self.timeSteps = timeSteps
    self.masses = Array(repeating: 0, count: bodies)
    self.positions = defaultState
    self.accelerations = defaultState
    self.velocities = defaultState

    let bodyData = lines.dropFirst().map({ $0.compactMap(Double.init) })

    guard bodyData.count == bodies * 3 else {
      return nil
    }

    for n in 0..<bodies {
      masses[n] = bodyData[0 + n * 3][0]

      guard let position = Vector(array: bodyData[1 + n * 3]),
            let velocity = Vector(array: bodyData[2 + n * 3]) else {
        return nil
      }

      positions[n] = position
      velocities[n] = velocity
    }
  }

  private func computeAccelerations() {
    for i in 0..<numBodies {
      accelerations[i] = .origin

      for j in 0..<numBodies where i != j {
        let t = gravitationalConstant * masses[j] / pow((positions[i] - positions[j]).mod(), 3)
        accelerations[i] = accelerations[i] + (positions[j] - positions[i]) * t
      }
    }
  }

  private func resolveCollisions() {
    for i in 0..<numBodies {
      for j in 0..<numBodies where positions[i] == positions[j] {
        velocities.swapAt(i, j)
      }
    }
  }

  private func computeVelocities() {
    for i in 0..<numBodies {
      velocities[i] = velocities[i] + accelerations[i]
    }
  }

  private func computePositions() {
    for i in 0..<numBodies {
      positions[i] = positions[i] + velocities[i] + accelerations[i] * 0.5
    }
  }

  public func printState() {
    for i in 0..<numBodies {
      print("Body \(i + 1): \(positions[i])  |  \(velocities[i])")
    }
  }

  public func simulate() {
    computeAccelerations()
    computePositions()
    computeVelocities()
    resolveCollisions()
  }
}

guard let sim = NBody(file: "input.txt") else {
  fatalError()
}

print()
print("Body   :      x          y          z    |     vx         vy         vz")

for i in 0..<sim.timeSteps {
  print("Step \(i + 1)")
  sim.simulate()
  sim.printState()
  print()
}

```


{{out}}

<pre style="height: 20em; overflow: auto;">Input file:
0.01 3 20
1
0.000000 0.000000 0.000000
0.010000 0.000000 0.000000
0.100000
1.000000 1.000000 0.000000
0.000000 0.000000 0.020000
0.001000
0.000000 1.000000 1.000000
0.010000 -0.010000 -0.010000

Body   :      x          y          z    |     vx         vy         vz
Step 1
Body 1: 0.010177	0.000179	0.000002  |  0.010354	0.000357	0.000004
Body 2: 0.998230	0.998232	0.020002  |  -0.003539	-0.003536	0.020004
Body 3: 0.010177	0.988232	0.988055  |  0.010354	-0.013536	-0.013889

Step 2
Body 1: 0.020709	0.000718	0.000011  |  0.010710	0.000721	0.000014
Body 2: 0.992907	0.992896	0.039971  |  -0.007109	-0.007138	0.019935
Body 3: 0.020717	0.972888	0.972173  |  0.010727	-0.017153	-0.017876

Step 3
Body 1: 0.031600	0.001625	0.000034  |  0.011072	0.001094	0.000033
Body 2: 0.983985	0.983910	0.059834  |  -0.010735	-0.010835	0.019790
Body 3: 0.031643	0.953868	0.952235  |  0.011125	-0.020886	-0.021999

Step 4
Body 1: 0.042858	0.002913	0.000081  |  0.011443	0.001481	0.000060
Body 2: 0.971393	0.971163	0.079509  |  -0.014448	-0.014659	0.019561
Body 3: 0.042981	0.931039	0.928087  |  0.011552	-0.024772	-0.026299

Step 5
Body 1: 0.054492	0.004595	0.000160  |  0.011826	0.001884	0.000097
Body 2: 0.955030	0.954509	0.098909  |  -0.018278	-0.018649	0.019238
Body 3: 0.054766	0.904225	0.899522  |  0.012018	-0.028857	-0.030829

Step 6
Body 1: 0.066517	0.006691	0.000281  |  0.012224	0.002308	0.000145
Body 2: 0.934759	0.933760	0.117931  |  -0.022265	-0.022849	0.018806
Body 3: 0.067040	0.873197	0.866280  |  0.012530	-0.033199	-0.035655

Step 7
Body 1: 0.078950	0.009225	0.000456  |  0.012642	0.002759	0.000206
Body 2: 0.910400	0.908677	0.136456  |  -0.026454	-0.027316	0.018244
Body 3: 0.079856	0.837662	0.828023  |  0.013101	-0.037871	-0.040861

Step 8
Body 1: 0.091815	0.012227	0.000702  |  0.013086	0.003245	0.000284
Body 2: 0.881722	0.878958	0.154340  |  -0.030902	-0.032122	0.017523
Body 3: 0.093281	0.797239	0.784313  |  0.013749	-0.042975	-0.046559

Step 9
Body 1: 0.105140	0.015737	0.001035  |  0.013564	0.003775	0.000383
Body 2: 0.848429	0.844216	0.171401  |  -0.035684	-0.037362	0.016600
Body 3: 0.107405	0.751427	0.734579  |  0.014498	-0.048649	-0.052908

Step 10
Body 1: 0.118964	0.019805	0.001481  |  0.014085	0.004362	0.000509
Body 2: 0.810137	0.803953	0.187408  |  -0.040900	-0.043166	0.015414
Body 3: 0.122346	0.699554	0.678056  |  0.015384	-0.055097	-0.060138

Step 11
Body 1: 0.133337	0.024498	0.002071  |  0.014662	0.005025	0.000672
Body 2: 0.766343	0.757509	0.202050  |  -0.046687	-0.049720	0.013868
Body 3: 0.138268	0.640690	0.613685  |  0.016460	-0.062633	-0.068603

Step 12
Body 1: 0.148327	0.029907	0.002851  |  0.015317	0.005792	0.000888
Body 2: 0.716377	0.703998	0.214889  |  -0.053246	-0.057302	0.011810
Body 3: 0.155406	0.573482	0.539941  |  0.017816	-0.071782	-0.078886

Step 13
Body 1: 0.164025	0.036157	0.003887  |  0.016079	0.006709	0.001184
Body 2: 0.659310	0.642172	0.225282  |  -0.060887	-0.066351	0.008976
Body 3: 0.174112	0.495836	0.454475  |  0.019596	-0.083511	-0.092045

Step 14
Body 1: 0.180564	0.043437	0.005286  |  0.017000	0.007852	0.001613
Body 2: 0.593807	0.570186	0.232208  |  -0.070119	-0.077621	0.004875
Body 3: 0.194929	0.404136	0.353320  |  0.022038	-0.099890	-0.110265

Step 15
Body 1: 0.198150	0.052049	0.007234  |  0.018171	0.009372	0.002283
Body 2: 0.517817	0.485100	0.233878  |  -0.081861	-0.092550	-0.001535
Body 3: 0.218605	0.290860	0.228583  |  0.025314	-0.126661	-0.139210

Step 16
Body 1: 0.217126	0.062542	0.010117  |  0.019781	0.011614	0.003484
Body 2: 0.427899	0.381659	0.226654  |  -0.097974	-0.114332	-0.012913
Body 3: 0.244268	0.131956	0.057562  |  0.026013	-0.191148	-0.202831

Step 17
Body 1: 0.238346	0.076539	0.015221  |  0.022658	0.016380	0.006723
Body 2: 0.317489	0.248502	0.200967  |  -0.122846	-0.151982	-0.038461
Body 3: 0.075592	-0.559591	-0.487315  |  -0.363366	-1.191945	-0.886924

Step 18
Body 1: 0.263123	0.097523	0.026918  |  0.026898	0.025587	0.016672
Body 2: 0.173428	0.050424	0.112716  |  -0.165275	-0.244174	-0.138041
Body 3: -0.286241	-1.745597	-1.369528  |  -0.360299	-1.180066	-0.877501

Step 19
Body 1: 0.270854	0.113045	0.061923  |  -0.011436	0.005457	0.053339
Body 2: 0.199821	-0.093105	-0.208666  |  0.218061	-0.042882	-0.504723
Body 3: -0.646318	-2.924909	-2.246453  |  -0.359856	-1.178559	-0.876350

Step 20
Body 1: 0.258572	0.116046	0.112038  |  -0.013129	0.000544	0.046890
Body 2: 0.426346	-0.111425	-0.681150  |  0.234987	0.006241	-0.440245
Body 3: -1.006089	-4.103186	-3.122591  |  -0.359686	-1.177995	-0.875924


Process finished with exit code 0

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

set G 0.01
set epsilon 1e-12

proc acceleration.gravity {positions masses} {
    global G epsilon
    set i -1
    lmap position $positions mass $masses {
	incr i
	set dp2 [lmap p $position {expr 0.0}]
	set j -1
	foreach pj $positions mj $masses {
	    if {[incr j] == $i} continue
	    set dp [lmap p1 $position p2 $pj {expr {$p1-$p2}}]
	    set d3 [expr {
		sqrt(
		    [tcl::mathop::+ {*}[lmap p $dp {expr {$p ** 2}}]]
		    + $epsilon
		) ** 3
	    }]
	    # Add epsilon here?
	    set dp2 [lmap a $dp2 b $dp {expr {$a - $G*$mj*$b/$d3}}]
	}
	set dp2
    }
}

# The rest of the system; simple numeric solution of differential equations
proc velocity {velocities accelerations} {
    lmap v $velocities a $accelerations {
	lmap vi $v ai $a {expr {$vi + $ai}}
    }
}
proc position {positions velocities} {
    lmap p $positions v $velocities {
	lmap pi $p vi $v {expr {$pi + $vi}}
    }
}
proc timestep {masses positions velocities} {
    set accelerations [acceleration.gravity $positions $masses]
    set velocities [velocity $velocities $accelerations]
    set positions [position $positions $velocities]
    list $positions $velocities
}

# Combine to make a simulation engine
proc simulate {masses positions velocities {steps 10}} {
    set p $positions
    set v $velocities
    for {set i 0} {$i < $steps} {incr i} {
	lassign [timestep $masses $p $v] p v
	puts [lmap pos $p {format (%.5f,%.5f,%.5f) {*}$pos}]
    }
}
```

Demonstrating for 20 steps:

```tcl
set m {1 .1 .001}
set p {{0 0 0} {1 1 0} {0 1 1}}
set v {{0.01 0 0} {0 0 0.02} {0.01 -0.01 -0.01}}
simulate $m $p $v 20
```

{{out}}

```txt

(0.01035,0.00036,0.00000) (0.99646,0.99646,0.02000) (0.01035,0.98646,0.98611)
(0.02107,0.00108,0.00002) (0.98934,0.98931,0.03994) (0.02108,0.96930,0.96822)
(0.03214,0.00218,0.00005) (0.97856,0.97843,0.05973) (0.03221,0.94837,0.94617)
(0.04359,0.00367,0.00011) (0.96402,0.96368,0.07928) (0.04377,0.92350,0.91977)
(0.05544,0.00557,0.00021) (0.94559,0.94487,0.09851) (0.05581,0.89447,0.88875)
(0.06768,0.00790,0.00036) (0.92308,0.92176,0.11729) (0.06837,0.86100,0.85279)
(0.08036,0.01070,0.00057) (0.89626,0.89406,0.13548) (0.08152,0.82271,0.81146)
(0.09350,0.01400,0.00086) (0.86482,0.86136,0.15292) (0.09535,0.77910,0.76420)
(0.10714,0.01786,0.00126) (0.82839,0.82318,0.16939) (0.10996,0.72951,0.71025)
(0.12133,0.02234,0.00179) (0.78643,0.77885,0.18457) (0.12552,0.67304,0.64858)
(0.13614,0.02754,0.00251) (0.73827,0.72746,0.19806) (0.14224,0.60833,0.57768)
(0.15167,0.03357,0.00346) (0.68293,0.66775,0.20924) (0.16048,0.53332,0.49521)
(0.16805,0.04065,0.00476) (0.61899,0.59779,0.21710) (0.18075,0.44439,0.39722)
(0.18551,0.04908,0.00659) (0.54422,0.51449,0.21990) (0.20386,0.33411,0.27581)
(0.20445,0.05946,0.00934) (0.45470,0.41210,0.21398) (0.22971,0.17975,0.10892)
(0.22573,0.07337,0.01421) (0.34228,0.27744,0.18934) (0.19746,-0.27259,-0.30685)
(0.25165,0.09535,0.02601) (0.18355,0.06171,0.09512) (0.16823,-0.69092,-0.69109)
(0.21467,0.08626,0.10162) (0.65371,0.15666,-0.63735) (0.13969,-1.10220,-1.06883)
(0.17838,0.07727,0.17608) (1.11702,0.25051,-1.35831) (0.11149,-1.51049,-1.44390)
(0.14224,0.06831,0.25028) (1.57874,0.34406,-2.07666) (0.08347,-1.91722,-1.81757)

```

