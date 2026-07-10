#!/bin/bash
# install_python_deps.sh
# Installation script for ConnectomeInfluenceCalculator Python dependencies
# Part of the influencer R package - installs into r-reticulate environment

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}🧠 ConnectomeInfluenceCalculator Installation Script${NC}"
echo -e "${BLUE}====================================================${NC}"
echo ""

# Pinned release of the ConnectomeInfluenceCalculator Python library that this
# version of influencer targets. Keep this in lockstep with the
# SystemRequirements line in DESCRIPTION. Must be an existing git tag/branch in
# the upstream repo; override with IC_VERSION=... to test against another ref.
IC_REPO="https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator.git"
IC_VERSION="${IC_VERSION:-v2.0.0}"
echo -e "${BLUE}Targeting ConnectomeInfluenceCalculator ${IC_VERSION}${NC}"
echo ""

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to fix pyproject.toml license format
fix_pyproject_toml() {
    local repo_dir="$1"
    local pyproject_path="$repo_dir/pyproject.toml"
    
    if [ -f "$pyproject_path" ]; then
        echo -e "${YELLOW}🔧 Fixing pyproject.toml license format...${NC}"
        # Fix the license format from string to object format
        sed -i.bak 's/^license = "BSD-3-Clause"/license = {text = "BSD-3-Clause"}/' "$pyproject_path"
        echo -e "${GREEN}✅ pyproject.toml license format fixed${NC}"
        return 0
    else
        echo -e "${YELLOW}⚠️  pyproject.toml not found at $pyproject_path${NC}"
        return 1
    fi
}

# Function to try simple pip installation
try_simple_pip_install() {
    echo -e "${YELLOW}🚀 Attempting simple pip installation...${NC}"
    
    # Clone repository to temporary directory
    TEMP_IC_DIR="/tmp/ConnectomeInfluenceCalculator_$$"
    
    if git clone --branch "$IC_VERSION" --depth 1 "$IC_REPO" "$TEMP_IC_DIR"; then
        echo -e "${GREEN}✅ Repository cloned successfully${NC}"
        
        # Fix pyproject.toml if needed
        fix_pyproject_toml "$TEMP_IC_DIR"
        
        echo -e "${YELLOW}📦 Installing ConnectomeInfluenceCalculator with pip...${NC}"
        
        # Try pip installation in r-reticulate environment
        if conda run -n r-reticulate python3 -m pip install "$TEMP_IC_DIR"; then
            echo -e "${GREEN}✅ Simple pip installation successful!${NC}"
            
            # Test the installation
            if conda run -n r-reticulate python -c "from InfluenceCalculator import InfluenceCalculator; print('✅ Import successful!')"; then
                echo -e "${GREEN}🎉 Installation and import test successful!${NC}"
                
                # Cleanup
                rm -rf "$TEMP_IC_DIR"
                
                # Write simple environment info
                cat > "$HOME/.influencer_env" << EOF
PYTHON_ENV=r-reticulate
INSTALL_METHOD=simple_pip
EOF
                
                echo -e "${BLUE}✅ Configuration saved to ~/.influencer_env${NC}"
                return 0
            else
                echo -e "${RED}❌ Import test failed${NC}"
                rm -rf "$TEMP_IC_DIR"
                return 1
            fi
        else
            echo -e "${RED}❌ Simple pip installation failed${NC}"
            rm -rf "$TEMP_IC_DIR"
            return 1
        fi
    else
        echo -e "${RED}❌ Failed to clone repository${NC}"
        return 1
    fi
}

# Function to install with full PETSc/SLEPc setup
install_with_petsc_slepc() {
    echo -e "${YELLOW}🔨 Simple installation failed. Setting up PETSc/SLEPc dependencies...${NC}"
    
    # Check if conda is available
    if ! command_exists conda; then
        echo -e "${RED}❌ Error: conda not found${NC}"
        echo "   Please install conda or miniconda first"
        exit 1
    fi
    
    # Ensure conda solver is set to classic
    conda config --set solver classic 2>/dev/null || true
    
    # Create or update r-reticulate environment if it doesn't exist
    if ! conda env list | grep -q "^r-reticulate "; then
        echo -e "${YELLOW}📦 Creating r-reticulate environment...${NC}"
        conda create -n r-reticulate python=3.11 -y
    fi
    
    echo -e "${YELLOW}🔍 Checking for PETSc and SLEPc in PATH...${NC}"
    
    # Check if PETSc/SLEPc are already available
    petsc_found=false
    slepc_found=false
    
    if command_exists petsc-config || command_exists PETSc-config; then
        petsc_found=true
        echo -e "${GREEN}✅ PETSc found in PATH${NC}"
    fi
    
    if command_exists slepc-config || command_exists SLEPc-config; then
        slepc_found=true
        echo -e "${GREEN}✅ SLEPc found in PATH${NC}"
    fi
    
    if [ "$petsc_found" = false ] || [ "$slepc_found" = false ]; then
        echo -e "${YELLOW}⚠️  PETSc/SLEPc not found in PATH${NC}"
        
        # Try Homebrew first
        if command_exists brew; then
            echo -e "${YELLOW}🍺 Attempting installation via Homebrew...${NC}"
            
            if ! brew list petsc >/dev/null 2>&1; then
                echo "   Installing PETSc..."
                brew install petsc 2>/dev/null || echo -e "${YELLOW}⚠️  Homebrew PETSc installation failed${NC}"
            fi
            
            if ! brew list slepc >/dev/null 2>&1; then
                echo "   Installing SLEPc..."
                brew install slepc 2>/dev/null || echo -e "${YELLOW}⚠️  Homebrew SLEPc installation failed${NC}"
            fi
        else
            echo -e "${YELLOW}⚠️  Homebrew not available, will use conda packages${NC}"
        fi
    fi
    
    # Install Python packages in r-reticulate environment
    echo -e "${YELLOW}📦 Installing Python packages...${NC}"
    
    # Install petsc4py and slepc4py
    echo "   Installing petsc4py and slepc4py..."
    if conda install -n r-reticulate -c conda-forge petsc4py slepc4py -y; then
        echo -e "${GREEN}✅ PETSc/SLEPc Python packages installed${NC}"
    else
        echo -e "${RED}❌ Failed to install PETSc/SLEPc Python packages${NC}"
        exit 1
    fi
    
    # Install other dependencies
    echo "   Installing additional dependencies..."
    conda install -n r-reticulate -c conda-forge pandas numpy bidict -y
    
    echo -e "${YELLOW}🧠 Installing ConnectomeInfluenceCalculator...${NC}"
    
    # Clone and install ConnectomeInfluenceCalculator
    TEMP_IC_DIR="/tmp/ConnectomeInfluenceCalculator_$$"
    if git clone --branch "$IC_VERSION" --depth 1 "$IC_REPO" "$TEMP_IC_DIR"; then
        echo "   Installing ConnectomeInfluenceCalculator..."
        
        # Fix pyproject.toml if needed
        fix_pyproject_toml "$TEMP_IC_DIR"
        
        # Install using conda run
        if conda run -n r-reticulate python -m pip install "$TEMP_IC_DIR"; then
            echo -e "${GREEN}✅ ConnectomeInfluenceCalculator installed successfully${NC}"
        else
            echo -e "${RED}❌ Failed to install ConnectomeInfluenceCalculator${NC}"
            rm -rf "$TEMP_IC_DIR"
            exit 1
        fi
        
        # Cleanup
        rm -rf "$TEMP_IC_DIR"
    else
        echo -e "${RED}❌ Failed to clone ConnectomeInfluenceCalculator${NC}"
        exit 1
    fi
    
    # Test the installation
    echo -e "${YELLOW}✅ Testing installation...${NC}"
    
    conda run -n r-reticulate python -c "
import sys
print(f'Python version: {sys.version}')
print('Testing imports...')

try:
    import petsc4py
    print('✅ petsc4py imported successfully')
except ImportError as e:
    print(f'❌ petsc4py import failed: {e}')
    sys.exit(1)

try:
    import slepc4py  
    print('✅ slepc4py imported successfully')
except ImportError as e:
    print(f'❌ slepc4py import failed: {e}')
    sys.exit(1)

try:
    from InfluenceCalculator import InfluenceCalculator
    print('✅ InfluenceCalculator imported successfully')
except ImportError as e:
    print(f'❌ InfluenceCalculator import failed: {e}')
    sys.exit(1)

print('🎉 All imports successful!')
"
    
    # Write environment info to file
    cat > "$HOME/.influencer_env" << EOF
PYTHON_ENV=r-reticulate
INSTALL_METHOD=full_petsc_slepc
EOF
    
    echo -e "${BLUE}✅ Configuration saved to ~/.influencer_env${NC}"
}

# Main installation logic
echo -e "${YELLOW}🐍 Setting up Python environment (r-reticulate)...${NC}"

# Check if conda is available
if ! command_exists conda; then
    echo -e "${RED}❌ Error: conda not found${NC}"
    echo "   Please install conda or miniconda first"
    exit 1
fi

# Fix conda solver issues by using classic solver
echo -e "${YELLOW}🔧 Configuring conda solver...${NC}"
conda config --set solver classic 2>/dev/null || true
echo "   Set conda solver to 'classic' to avoid libmamba issues"

# Ensure r-reticulate environment exists
if conda env list | grep -q "^r-reticulate "; then
    echo "   r-reticulate environment already exists"
else
    echo "   Creating r-reticulate environment..."
    conda create -n r-reticulate python=3.11 -y
fi

# Try simple pip installation first
if try_simple_pip_install; then
    echo ""
    echo -e "${GREEN}🎉 Simple installation completed successfully!${NC}"
else
    echo ""
    echo -e "${YELLOW}💡 Simple pip installation failed, trying full setup...${NC}"
    install_with_petsc_slepc
    echo ""
    echo -e "${GREEN}🎉 Full installation completed successfully!${NC}"
fi

echo ""
echo -e "${BLUE}📝 Environment Configuration:${NC}"
echo "   Python environment: r-reticulate"
echo ""
echo -e "${YELLOW}💡 To use this installation in R:${NC}"
echo "   The influencer package will automatically use the r-reticulate environment"
echo "   No manual configuration needed"
echo ""