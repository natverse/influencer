# ---- PRELIM: Set all variables up front for easy copying/editing ----
export PETSC_VERSION=3.23.4
export SLEPC_VERSION=3.23.2
export PETSC_SRC=$HOME/petsc-$PETSC_VERSION-src
export PETSC_INSTALL=$HOME/petsc-$PETSC_VERSION
export SLEPC_SRC=$HOME/slepc-$SLEPC_VERSION-src
export SLEPC_INSTALL=$HOME/slepc-$SLEPC_VERSION

# ---- 1. Get PETSc (Source) ----
cd $HOME
git clone -b v$PETSC_VERSION https://gitlab.com/petsc/petsc.git $PETSC_SRC
cd $PETSC_SRC

# ---- 2. Configure PETSc ----
export PETSC_DIR=$PETSC_SRC
./configure --prefix=$PETSC_INSTALL \
    --with-cc=mpicc --with-cxx=mpicxx --with-fc=mpif90 \
    --download-f2cblaslapack=1 --with-debugging=0

# ---- 3. Build and Install PETSc ----
make all
make install

# (Optional) Confirm PETSc libraries installed
ls $PETSC_INSTALL/lib

# ---- 4. Get SLEPc (Source) ----
cd $HOME
git clone -b v$SLEPC_VERSION https://gitlab.com/slepc/slepc.git $SLEPC_SRC
cd $SLEPC_SRC

# ---- 5. Configure SLEPc ----
export PETSC_DIR=$PETSC_INSTALL
./configure --prefix=$SLEPC_INSTALL

# ---- 6. Build and Install SLEPc ---
make SLEPC_DIR=$SLEPC_SRC PETSC_DIR=$PETSC_INSTALL
make install SLEPC_DIR=$SLEPC_SRC PETSC_DIR=$PETSC_INSTALL

# ---- 7. (Optional) Test SLEPc install ----
export SLEPC_DIR=$SLEPC_INSTALL
cd $SLEPC_DIR
make check

# ---- 8. (Optional) Add these environment lines to ~/.bashrc for future use ----
echo "export PETSC_DIR=$PETSC_INSTALL" >> ~/.bashrc
echo "export SLEPC_DIR=$SLEPC_INSTALL" >> ~/.bashrc