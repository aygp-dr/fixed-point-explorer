# GitHub Issue: Add Expect Scripts for End-to-End Integration Testing

## Title
feat(test): Add expect scripts for CLI integration testing with contract validation

## Labels
- enhancement
- testing
- observer
- meta

## Description

### Summary
Implement expect scripts for automated end-to-end testing of the Fixed Point Explorer CLI interactions, ensuring consistent behavior across different Scheme implementations and providing contract validation for inputs, outputs, and interactive sessions.

### Motivation
Currently, the project has unit tests and cross-implementation tests, but lacks automated integration testing for interactive REPL sessions and CLI interactions. Expect scripts would provide:
- Automated verification of REPL behavior
- Contract validation for CLI arguments
- Prompt expectation testing
- Interactive command support validation
- Regression testing for user workflows

### Proposed Implementation

#### 1. Core Expect Framework
```tcl
# test/expect/framework.exp
proc test_repl {impl command expected} {
    spawn $impl
    expect ">"
    send "$command\r"
    expect {
        $expected { pass "$impl: $command" }
        timeout { fail "$impl: $command (timeout)" }
    }
}
```

#### 2. CLI Argument Contract Testing
```tcl
# test/expect/cli-contracts.exp
# Test version 0.1.0 -> 1.0 compatibility
proc test_cli_args {version} {
    spawn ./scripts/demo.sh --version=$version
    expect {
        -re "Version: $version" { pass "Version $version supported" }
        default { fail "Version $version not recognized" }
    }
}

# Test argument validation
foreach arg {--help --version --quiet --verbose} {
    test_cli_argument $arg
}
```

#### 3. Prompt Expectation Testing
```tcl
# test/expect/prompt-tests.exp
proc test_prompt_sequence {impl} {
    spawn $impl
    
    # Test basic Y combinator
    expect ">"
    send "(load \"src/portable/y-combinator.scm\")\r"
    expect "loaded"
    
    # Test factorial computation
    send "(define fact (Y-explicit (lambda (f) (lambda (n) (if (zero? n) 1 (* n (f (- n 1))))))))\r"
    expect "#<procedure"
    
    send "(fact 5)\r"
    expect "120"
}
```

#### 4. Interactive Command Support (0.0 - 1.0)
```tcl
# test/expect/interactive-commands.exp
# Version progression testing
set versions {0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0}

foreach v $versions {
    test_case "Version $v compatibility" {
        # Test backward compatibility
        spawn ./fixed-point-explorer --compat=$v
        expect "Compatibility mode: $v"
        
        # Test core features available in version
        if {$v >= 0.3} {
            send ",help\r"
            expect "Available commands"
        }
        
        if {$v >= 0.5} {
            send ",trace fact\r"
            expect "Tracing enabled"
        }
        
        if {$v >= 0.7} {
            send ",benchmark fact 1000\r"
            expect -re "Time: \\d+ms"
        }
    }
}
```

#### 5. Expected Outcomes Matrix
```tcl
# test/expect/outcomes.exp
array set expected_outcomes {
    "fibonacci 10"     "55"
    "factorial 5"      "120"
    "map double"       "(2 4 6 8)"
    "filter even?"     "(2 4)"
    "fold-left + 0"    "10"
}

proc test_all_implementations {} {
    foreach impl {guile chez racket} {
        foreach {test expected} [array get expected_outcomes] {
            test_outcome $impl $test $expected
        }
    }
}
```

### Test Structure
```
test/
├── expect/
│   ├── framework.exp          # Core testing utilities
│   ├── cli-contracts.exp      # CLI argument validation
│   ├── prompt-tests.exp       # REPL interaction tests
│   ├── interactive-commands.exp # Version compatibility
│   ├── outcomes.exp           # Expected results matrix
│   └── run-all.exp           # Test orchestration
```

### Makefile Integration
```makefile
test-expect: ## Run expect integration tests
	@echo "Running expect integration tests..."
	@cd test/expect && expect run-all.exp

test-expect-verbose: ## Run expect tests with detailed output
	@cd test/expect && expect -d run-all.exp

test-all: test test-lean-all test-expect ## Run all tests including expect
```

### Contract Specifications

1. **CLI Arguments Contract**
   - `--help`: Must display usage information
   - `--version`: Must show version (0.1.0 - 1.0)
   - `--repl={guile,chez,racket}`: Select implementation
   - `--quiet`: Suppress non-essential output
   - `--verbose`: Enable debug output

2. **Prompt Expectations Contract**
   - Initial prompt: `> ` or implementation-specific
   - Error format: `Error: <message>`
   - Result format: Value on new line
   - Multi-line input: Continuation with `...`

3. **Interactive Commands Contract**
   - `,help`: List available commands
   - `,quit` or `,exit`: Clean termination
   - `,load <file>`: Load source file
   - `,trace <func>`: Enable tracing (v0.5+)
   - `,time <expr>`: Time expression (v0.6+)
   - `,benchmark <func> <n>`: Benchmark (v0.7+)

### Success Criteria
- [ ] All Scheme implementations pass core tests
- [ ] CLI contracts validated for all versions
- [ ] Interactive commands work as specified
- [ ] Expect scripts run in CI/CD pipeline
- [ ] Documentation updated with test examples
- [ ] Performance benchmarks captured

### Dependencies
- expect/tcl installed (`gmake deps` should check)
- All Scheme implementations available
- Test data files in place

### Notes
- Use timeout values appropriate for CI environments
- Consider Docker containers for consistent testing
- Add hooks for custom implementation testing
- Support for future version beyond 1.0

### References
- [Expect documentation](https://www.tcl.tk/man/expect5.31/expect.1.html)
- [TCL testing best practices](https://wiki.tcl-lang.org/page/Testing)
- Project's existing test framework in `test/`