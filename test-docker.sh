#!/usr/bin/env bash
# Test script for Colima & Docker functionality

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_success() { echo -e "${GREEN}✓ $1${NC}"; }
print_warning() { echo -e "${YELLOW}⚠ $1${NC}"; }
print_error() { echo -e "${RED}✗ $1${NC}"; }
print_info() { echo -e "${BLUE}ℹ $1${NC}"; }

# Track test results
TESTS_PASSED=0
TESTS_FAILED=0

run_test() {
    local test_name="$1"
    local test_command="$2"

    print_warning "Testing: $test_name"
    if eval "$test_command" &>/dev/null; then
        print_success "$test_name passed"
        ((TESTS_PASSED++))
        return 0
    else
        print_error "$test_name failed"
        ((TESTS_FAILED++))
        return 1
    fi
}

echo -e "${BLUE}===================================================${NC}"
echo -e "${BLUE}Testing Colima & Docker Setup${NC}"
echo -e "${BLUE}===================================================${NC}\n"

# Test 1: Colima is installed
run_test "Colima installation" "command -v colima"

# Test 2: Docker is installed
run_test "Docker installation" "command -v docker"

# Test 3: Colima is running
if run_test "Colima running" "colima status"; then
    COLIMA_STATUS=$(colima status 2>&1 | grep -o 'colima is running.*')
    print_info "$COLIMA_STATUS"
fi

# Test 4: Docker daemon is accessible
if run_test "Docker daemon" "docker info"; then
    DOCKER_VERSION=$(docker version --format '{{.Server.Version}}' 2>/dev/null)
    print_info "Docker server version: $DOCKER_VERSION"
fi

# Test 5: Docker can list containers
if run_test "Docker ps command" "docker ps"; then
    CONTAINER_COUNT=$(docker ps -q | wc -l | tr -d ' ')
    print_info "Running containers: $CONTAINER_COUNT"
fi

# Test 6: Docker context is set to colima
if run_test "Docker context" "docker context show | grep -q colima"; then
    DOCKER_CONTEXT=$(docker context show)
    print_info "Docker context: $DOCKER_CONTEXT"
fi

# Test 7: Docker can pull and run a test container (optional, commented out by default)
# Uncomment to enable this test
# if run_test "Docker run test" "docker run --rm hello-world"; then
#     print_info "Docker can successfully run containers"
# fi

# Test 8: Docker socket is accessible
if [ -S "$HOME/.colima/default/docker.sock" ]; then
    print_success "Docker socket exists at ~/.colima/default/docker.sock"
    ((TESTS_PASSED++))
else
    print_error "Docker socket not found at ~/.colima/default/docker.sock"
    ((TESTS_FAILED++))
fi

# Test 9: Docker Compose is available
# Check plugin first (preferred), then standalone
if docker compose version &>/dev/null; then
    if run_test "Docker Compose (plugin)" "docker compose version"; then
        COMPOSE_VERSION=$(docker compose version --short 2>/dev/null || docker compose version 2>/dev/null | grep -oE 'v[0-9]+\.[0-9]+\.[0-9]+' | head -1)
        print_info "Docker Compose plugin version: $COMPOSE_VERSION"
    fi
elif command -v docker-compose &>/dev/null; then
    if run_test "Docker Compose (standalone)" "docker-compose version"; then
        COMPOSE_VERSION=$(docker-compose version --short 2>/dev/null)
        print_info "Docker Compose standalone version: $COMPOSE_VERSION"
    fi
else
    print_warning "Testing: Docker Compose"
    print_info "Docker Compose not found (optional)"
fi

# Summary
echo -e "\n${BLUE}===================================================${NC}"
echo -e "${BLUE}Test Results${NC}"
echo -e "${BLUE}===================================================${NC}\n"

TOTAL_TESTS=$((TESTS_PASSED + TESTS_FAILED))
echo -e "Total tests: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
echo -e "${RED}Failed: $TESTS_FAILED${NC}\n"

if [ $TESTS_FAILED -eq 0 ]; then
    print_success "All tests passed! Docker and Colima are working correctly."
    exit 0
else
    print_error "Some tests failed. Please review the output above."
    exit 1
fi
