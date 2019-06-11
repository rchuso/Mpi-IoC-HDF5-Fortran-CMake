Modern Fortran with Polymorphism, CMake, MPI, HDF5, Inversion of Control
=============

I'm offering this repository as a springboard for anyone else who is having difficulty with CMake, HDF5, MPI, and modern Fortran.

This CMake project builds a modern Fortran executable that uses MPI and HDF5.


Requirements
-------------

The Fortran requires a recent compiler. I used 7.3 (or .4) for the development. Many of the modern features are not available in versions prior to gfortran-6. To install gfortran-7 on Ubuntu or Mint, try this:

  - sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test
  - sudo apt-get update -y
  - sudo apt-get install -y gfortran-7

You will also need a relatively recent version of OpenMPI (there is a very slight difference in how to reference the polymorphic base class when using mpich - I've successfully used both, and the code in MsgBase.f90 declares the integer variable "baseId" which is required by OpenMPI but not by mpich, but all my recent testing has been with OpenMPI). When the packet is sent with the MPI_SEND function, this variable is referenced for OpenMPI, but unnecessary with mpich.

You will also need the HDF5 Fortran libraries installed - something that's not done by default when installing HDF5 - you have to explicitly require the Fortran to be installed.

You will also need a relatively recent version of CMake. I developed this with CMake version 3.10, but any recent version should work with Fortran.


Running
-------------

Once the executable is built, you can run it like any other MPI application:
  - mpiexec -n 3 bin/TestMpiIocHdf5

If all worked correctly, you should see something like this:

     Capabilities (to examine, process, use):
               1 MCT:: rank=0 size=3 datatype=75 RAM free:26057 total:32036 cores:8 Eshnunna
               2 MCT:: rank=2 size=3 datatype=75 RAM free:26057 total:32036 cores:8 Eshnunna
               3 MCT:: rank=1 size=3 datatype=75 RAM free:26057 total:32036 cores:8 Eshnunna
    1:3 rx:MXT:: myPi=2.718 datatype=76 from:0 tag:37
    2:3 rx:MXT:: myPi=2.718 datatype=76 from:0 tag:37
    0:3 rx:MXT:: myPi=2.718 datatype=76 from:1 tag:37
    0:3 rx:MXT:: myPi=2.718 datatype=76 from:2 tag:37
    %-I- [-1]    354: HD_constructor    ==> filename: hyper.h5
    %-I- [-1]    364: HD_dsestructor    ==> closing: hyper.h5


The Application and Structure
-------------

The main Fortran program is TestMpiIocHdf5.f90, and it is in the "code/apps" folder. All the code is under the "code" folder, and I do that to simplify the directory structure. The CMake script creates the "bin", "mod", and "lib" folders. The executable is put in the "bin" folder, the intermediate Fortran module files go in the "mod", and the created libraries go in the "lib" folder.

Under the "code" folder is the "apps" folder which holds the main program(s). There are also two sub-folders in "code" that are where the source code resides for the two libraries - one for the MPI_IOC, called "libMpi", and the other, called "libHdf5", for the HDF5. And the CMake scripts create both shared and static libraries for each.

The "libMpi" library wraps MPI with inversion-of-control, which I've used with great success (well, the earlier C version anyway) to create a suite of seismic data processing applications that run on the world's largest privately-owned supercomputers (like the one owned by Total - total.com). The code here is a completely new creation, and probably even faster than the earlier versions, but I don't have access to a supercomputer to test that claim. (This one removes the need to have a small packet for use within the *layer* to coordinate the message traffic.)


MPI
-------------

Applications that want to make use of the standard software paradigm "inversion-of-control" (or IoC), are different from what most people think of when writing MPI applications. It's my experience that scientists who write MPI applications, or even compiler and library designers, think the application should divide the work and send work to the worker nodes. I've written this IoC layer to allow the worker nodes to request data when they're ready, instead of having the control node (rank=0) divide the workload. In doing so, the workers request work when they have capacity, and the data transmission is more randomly split than if the work is sent out all at once. This allows the application to adjust dynamically to the workload or even other applications that may also be using the computer. In fact, this works so well, I've seen my applications crowd out other applications, while mine keeps the CPU on that node at close to 100%. Testing one of these applications on 2000 nodes showed a linear increase over smaller tests. I understand some of these applications have been run on 86000 nodes (that's eighty six thousand nodes). It also allows me to change the functionining of any node by simply sending that node a message telling it what to do.

This layer allows me to send a data packet to any node at any time without prior coordination. This greatly simplifies the creation of MPI applications, as each node only needs to respond to received messages. When the application starts the IoC layer, the layer gets the capabilities of the node on which it's running and sends this to the control node for storage and reference. This allows the control node to initially designate the functioning of other nodes: if 16 nodes report the same hostname, for the SRME3D application for instance, I designate one of them as a file reader and the others of that same name as users of the data from the file reader. This keeps their local MPI communications off the backplane, and stops that potential interferrence on the network, while keeping the number of file pointers in use under control. I wrote a bit more about this on my mjollnir.com web site, and there is a recent version of the C code as well.

The test application also comes with a test message, which is transferred between the nodes. On one relatively arbitrary message receipt, the application stops the IoC layer, and control is returned to the main program.


HDF5
-------------

The second part of the test application creates a simple HDF5 output file. The HDF5 functions for Fortran are significantly more complicated than for Python, so I pushed that complexity down into the library. The user only needs to create an object of the HDType, invoke the constructor, and begin reading or writing - with the most pythonic Fortran I could envisage.


Disclaimer
-------------

I'm certainly not an expert on all these fields, and I'm definitely open to suggestions, and I can extend the HDF5 library, if needed - it currently only supports the reading and writing of INTEGER (int32 and int64) and REAL (real32 and real64) arrays of 1, 2, or 3 dimensions. This was all I needed for my scientific work, but it can be extended... but now there's another repository on GitHub that provides what looks like a more complete implementation, but it's not as "slick" as mine (in my humble opinion.) When some form of templating or generic is available for Fortran, I'll simplify and extend the functionality.


