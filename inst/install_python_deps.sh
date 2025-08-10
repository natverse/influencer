#!/bin/bash
# install_python_deps.sh
# Installation script for ConnectomeInfluenceCalculator Python dependencies
# Part of the influencer R package

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

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Source common shell profiles to ensure PATH is set correctly
if [ -f "$HOME/.zshrc" ]; then
    source "$HOME/.zshrc" 2>/dev/null || true
elif [ -f "$HOME/.bashrc" ]; then
    source "$HOME/.bashrc" 2>/dev/null || true
elif [ -f "$HOME/.bash_profile" ]; then
    source "$HOME/.bash_profile" 2>/dev/null || true
fi

# Check prerequisites
echo -e "${YELLOW}🔍 Checking prerequisites...${NC}"
if ! command_exists brew; then
    echo -e "${RED}❌ Error: Homebrew is not installed. Please install it first:${NC}"
    echo "   /bin/bash -c \"\$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
    exit 1
fi

if ! command_exists uv; then
    echo -e "${RED}❌ Error: UV is not installed. Please install it first:${NC}"
    echo "   brew install uv"
    exit 1
fi

# Check for Xcode Command Line Tools
echo "   Checking Xcode Command Line Tools..."
if ! xcode-select -p >/dev/null 2>&1; then
    echo -e "${RED}❌ Error: Xcode Command Line Tools not installed.${NC}"
    echo "   Please install them first with:"
    echo "   xcode-select --install"
    echo ""
    echo "   Then run this script again after the installation completes."
    exit 1
fi

# Verify Command Line Tools are properly installed at the expected location
if [ ! -d "/Library/Developer/CommandLineTools" ]; then
    echo -e "${RED}❌ Error: Xcode Command Line Tools not properly installed.${NC}"
    echo "   Expected location: /Library/Developer/CommandLineTools"
    echo "   Please reinstall with: xcode-select --install"
    echo ""
    echo "   If you have Xcode.app installed, you may need to run:"
    echo "   sudo xcode-select --switch /Library/Developer/CommandLineTools"
    exit 1
fi

echo -e "${GREEN}✅ Prerequisites satisfied${NC}"
echo ""

# Install core dependencies with Homebrew
echo -e "${YELLOW}🍺 Installing PETSc and SLEPc with Homebrew...${NC}"

# Function to try different installation methods
install_with_fallback() {
    local package=$1
    local package_name=$2
    
    if brew list "$package" >/dev/null 2>&1; then
        echo "   $package_name already installed"
        return 0
    fi
    
    echo "   Installing $package_name..."
    
    # First, check if there will be Xcode Command Line Tools issues
    local temp_output=$(mktemp)
    if brew install "$package" 2>&1 | tee "$temp_output" | grep -q "Xcode Command Line Tools"; then
        echo -e "${YELLOW}   ⚠️  Detected Xcode Command Line Tools issue${NC}"
        echo "   Skipping normal installation and trying from source..."
        rm -f "$temp_output"
        
        echo "   Installing $package_name from source (this may take 10-30 minutes)..."
        if brew install --build-from-source "$package"; then
            echo "   ✅ $package_name installed successfully from source"
            return 0
        else
            echo -e "${RED}   ❌ Source build failed${NC}"
        fi
    else
        # Check if normal installation succeeded
        if [ $? -eq 0 ]; then
            echo "   ✅ $package_name installed successfully"
            rm -f "$temp_output"
            return 0
        fi
    fi
    
    rm -f "$temp_output"
    
    # Final attempt with more specific error handling
    echo -e "${RED}❌ Error: Failed to install $package_name.${NC}"
    echo ""
    echo "   This is likely due to a Xcode Command Line Tools configuration issue."
    echo "   Please try the following solutions:"
    echo ""
    echo "   1. Reset Xcode Command Line Tools:"
    echo "      sudo xcode-select --reset"
    echo "      xcode-select --install"
    echo ""
    echo "   2. If you have Xcode.app installed, switch to it:"
    echo "      sudo xcode-select --switch /Applications/Xcode.app/Contents/Developer"
    echo ""
    echo "   3. Or ensure standalone tools are properly linked:"
    echo "      sudo xcode-select --switch /Library/Developer/CommandLineTools"
    echo ""
    echo "   4. After fixing the above, retry this script"
    echo ""
    echo "   Alternative: You may also try using conda instead:"
    echo "   R -e 'influencer::install_python_influence_calculator()'"
    echo ""
    return 1
}

# Install PETSc and SLEPc with fallback strategies
if ! install_with_fallback "petsc" "PETSc"; then
    exit 1
fi

if ! install_with_fallback "slepc" "SLEPc"; then
    exit 1
fi

# Set environment variables
echo -e "${YELLOW}🔧 Setting environment variables...${NC}"
export PETSC_DIR=$(brew --prefix petsc)
export SLEPC_DIR=$(brew --prefix slepc)

echo "   PETSC_DIR: $PETSC_DIR"
echo "   SLEPC_DIR: $SLEPC_DIR"
echo ""

# Determine environment name and location
ENV_NAME="influence-py"
ENV_PATH="$HOME/.local/share/uv/envs/$ENV_NAME"

# Create Python environment with UV
echo -e "${YELLOW}🐍 Creating Python environment with UV...${NC}"
echo "   Environment: $ENV_NAME"
echo "   Location: $ENV_PATH"

# Remove existing environment if it exists
if [ -d "$ENV_PATH" ]; then
    echo "   Removing existing environment..."
    rm -rf "$ENV_PATH"
fi

uv venv "$HOME/.local/share/uv/envs/$ENV_NAME" --python 3.13.1
echo ""

# Install Python packages
echo -e "${YELLOW}📦 Installing Python wrappers (petsc4py, slepc4py)...${NC}"

# Function to install Python packages with fallbacks
install_python_packages() {
    echo "   Installing petsc4py and slepc4py..."
    
    # Try installing with current PETSc/SLEPc installation
    if UV_PROJECT_ENVIRONMENT="$HOME/.local/share/uv/envs/$ENV_NAME" uv pip install petsc4py slepc4py 2>/dev/null; then
        echo "   ✅ Python packages installed successfully"
    else
        echo -e "${YELLOW}   ⚠️  Standard installation failed, trying alternative approach...${NC}"
        
        # Try installing with specific versions that might have better wheel support
        if UV_PROJECT_ENVIRONMENT="$HOME/.local/share/uv/envs/$ENV_NAME" uv pip install petsc4py==3.21.0 slepc4py==3.21.0 2>/dev/null; then
            echo "   ✅ Python packages installed with specific versions"
        else
            echo -e "${RED}   ❌ Failed to install Python wrappers${NC}"
            echo "   This may be due to PETSc/SLEPc installation issues."
            echo "   Try running the script again after fixing the Homebrew installation."
            return 1
        fi
    fi
    return 0
}

if ! install_python_packages; then
    exit 1
fi

echo -e "${YELLOW}🧠 Installing ConnectomeInfluenceCalculator from GitHub...${NC}"

# Function to install ConnectomeInfluenceCalculator with automatic fixes
install_influence_calculator() {
    # Try standard installation first
    echo "   Attempting standard installation..."
    if UV_PROJECT_ENVIRONMENT="$HOME/.local/share/uv/envs/$ENV_NAME" uv pip install git+https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator@v0.3 2>/dev/null; then
        echo "   ✅ Standard installation successful"
        return 0
    fi
    
    # If that fails, try with pyproject.toml fix
    echo -e "${YELLOW}   ⚠️  Standard installation failed, applying pyproject.toml fix...${NC}"
    
    # Clone and fix pyproject.toml
    TEMP_DIR="/tmp/ConnectomeInfluenceCalculator_fixed_$$"
    if git clone https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator.git "$TEMP_DIR" >/dev/null 2>&1; then
        echo "   Fixing pyproject.toml license format issue..."
        sed -i '' 's/^license = "BSD-3-Clause"/license = {text = "BSD-3-Clause"}/' "$TEMP_DIR/pyproject.toml"
        
        if UV_PROJECT_ENVIRONMENT="$HOME/.local/share/uv/envs/$ENV_NAME" uv pip install "$TEMP_DIR" 2>/dev/null; then
            echo "   ✅ Installation successful with pyproject.toml fix"
            rm -rf "$TEMP_DIR"
            return 0
        else
            echo -e "${RED}   ❌ Installation failed even with pyproject.toml fix${NC}"
            rm -rf "$TEMP_DIR"
            return 1
        fi
    else
        echo -e "${RED}   ❌ Could not clone repository for fixing${NC}"
        return 1
    fi
}

if ! install_influence_calculator; then
    echo -e "${RED}❌ Error: Failed to install ConnectomeInfluenceCalculator${NC}"
    echo "   This may be due to:"
    echo "   - Network connectivity issues"
    echo "   - Missing system dependencies"
    echo "   - Upstream repository issues"
    echo ""
    echo "   Try the conda-based installation instead:"
    echo "   R -e 'influencer::install_python_influence_calculator()'"
    exit 1
fi

echo ""

# Test installation
echo -e "${YELLOW}✅ Testing installation...${NC}"
UV_PROJECT_ENVIRONMENT="$HOME/.local/share/uv/envs/$ENV_NAME" uv run python -c "
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

echo ""
echo -e "${GREEN}🎉 Installation completed successfully!${NC}"
echo ""
echo -e "${BLUE}📝 Usage Instructions:${NC}"
echo "   1. In R, use this environment with:"
echo "      reticulate::use_virtualenv('$ENV_PATH')"
echo ""
echo "   2. Or set environment variables for the current session:"
echo "      export PETSC_DIR=$PETSC_DIR"
echo "      export SLEPC_DIR=$SLEPC_DIR"
echo ""
echo "   3. To activate this environment manually:"
echo "      source $ENV_PATH/bin/activate"
echo ""
echo -e "${BLUE}🔧 Environment Details:${NC}"
echo "   Name: $ENV_NAME"
echo "   Path: $ENV_PATH"
echo "   Python: $(UV_PROJECT_ENVIRONMENT="$ENV_PATH" uv run python --version)"
echo ""
echo -e "${YELLOW}💡 Tip: Add the export commands to your ~/.zshrc or ~/.bashrc for persistence${NC}"

#export PETSC_DIR=/usr/local/Cellar/petsc/3.23.5/
#export SLEPC_DIR=/usr/local/Cellar/slepc/3.23.2/
#uv add git+https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator --tag v0.3

