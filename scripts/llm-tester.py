#!/usr/bin/env python3
"""
LLM Test Harness for CNS Code Generation
Tests language models' ability to generate valid CNS code

Supports:
- xAI Grok (grok-2-latest, grok-beta, etc.)
- OpenAI (gpt-4, gpt-3.5-turbo, etc.) 
- Anthropic Claude (claude-3-opus, claude-3-sonnet, etc.)
- OpenRouter (any model via openrouter.ai)
"""

import argparse
import json
import os
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Optional, Dict, List, Tuple

try:
    import requests
except ImportError:
    print("ERROR: requests library not found. Install with: pip install requests")
    sys.exit(1)


class LLMClient:
    """Base class for LLM API clients"""
    
    def __init__(self, api_key: str, model: str):
        self.api_key = api_key
        self.model = model
        
    def generate(self, prompt: str, system_prompt: Optional[str] = None, max_tokens: int = 2000, temperature: float = 0.7) -> str:
        raise NotImplementedError()


class GrokClient(LLMClient):
    """Client for xAI's Grok API"""
    
    def __init__(self, api_key: str, model: str = "grok-2-latest"):
        super().__init__(api_key, model)
        self.base_url = "https://api.x.ai/v1"
        
    def generate(self, prompt: str, system_prompt: Optional[str] = None, max_tokens: int = 2000, temperature: float = 0.7) -> str:
        headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json"
        }
        
        messages = []
        if system_prompt:
            messages.append({"role": "system", "content": system_prompt})
        messages.append({"role": "user", "content": prompt})
        
        payload = {
            "messages": messages,
            "model": self.model,
            "temperature": temperature,
            "max_tokens": max_tokens
        }
        
        response = requests.post(
            f"{self.base_url}/chat/completions",
            headers=headers,
            json=payload,
            timeout=30
        )
        
        if response.status_code != 200:
            raise Exception(f"API Error {response.status_code}: {response.text}")
        
        result = response.json()
        return result["choices"][0]["message"]["content"]


class OpenAIClient(LLMClient):
    """Client for OpenAI API"""
    
    def __init__(self, api_key: str, model: str = "gpt-4"):
        super().__init__(api_key, model)
        self.base_url = "https://api.openai.com/v1"
        
    def generate(self, prompt: str, system_prompt: Optional[str] = None, max_tokens: int = 2000, temperature: float = 0.7) -> str:
        headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json"
        }
        
        messages = []
        if system_prompt:
            messages.append({"role": "system", "content": system_prompt})
        messages.append({"role": "user", "content": prompt})
        
        payload = {
            "messages": messages,
            "model": self.model,
            "temperature": temperature,
            "max_tokens": max_tokens
        }
        
        response = requests.post(
            f"{self.base_url}/chat/completions",
            headers=headers,
            json=payload,
            timeout=60
        )
        
        if response.status_code != 200:
            raise Exception(f"API Error {response.status_code}: {response.text}")
        
        result = response.json()
        return result["choices"][0]["message"]["content"]


class ClaudeClient(LLMClient):
    """Client for Anthropic Claude API"""
    
    def __init__(self, api_key: str, model: str = "claude-3-sonnet-20240229"):
        super().__init__(api_key, model)
        self.base_url = "https://api.anthropic.com/v1"
        
    def generate(self, prompt: str, system_prompt: Optional[str] = None, max_tokens: int = 2000, temperature: float = 0.7) -> str:
        headers = {
            "x-api-key": self.api_key,
            "Content-Type": "application/json",
            "anthropic-version": "2023-06-01"
        }
        
        payload = {
            "model": self.model,
            "max_tokens": max_tokens,
            "temperature": temperature,
            "messages": [{"role": "user", "content": prompt}]
        }
        
        if system_prompt:
            payload["system"] = system_prompt
        
        response = requests.post(
            f"{self.base_url}/messages",
            headers=headers,
            json=payload,
            timeout=60
        )
        
        if response.status_code != 200:
            raise Exception(f"API Error {response.status_code}: {response.text}")
        
        result = response.json()
        return result["content"][0]["text"]


class OpenRouterClient(LLMClient):
    """Client for OpenRouter (unified API for many models)"""
    
    def __init__(self, api_key: str, model: str):
        super().__init__(api_key, model)
        self.base_url = "https://openrouter.ai/api/v1"
        
    def generate(self, prompt: str, system_prompt: Optional[str] = None, max_tokens: int = 2000, temperature: float = 0.7) -> str:
        headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
            "HTTP-Referer": "https://github.com/jessecrouch/cns",
            "X-Title": "CNS LLM Tester"
        }
        
        messages = []
        if system_prompt:
            messages.append({"role": "system", "content": system_prompt})
        messages.append({"role": "user", "content": prompt})
        
        payload = {
            "model": self.model,
            "messages": messages,
            "temperature": temperature,
            "max_tokens": max_tokens
        }
        
        response = requests.post(
            f"{self.base_url}/chat/completions",
            headers=headers,
            json=payload,
            timeout=60
        )
        
        if response.status_code != 200:
            raise Exception(f"API Error {response.status_code}: {response.text}")
        
        result = response.json()
        return result["choices"][0]["message"]["content"]


class CNSTester:
    """Tests CNS code generation and validation"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.validator_path = project_root / "src" / "cns-validate"
        self.runner_path = project_root / "cns-run"
        
        # Try alternate locations if main ones don't exist
        if not self.runner_path.exists():
            self.runner_path = project_root / "src" / "cns-run"
        
        self.results_dir = project_root / "tests" / "llm-tests" / "results"
        self.generated_dir = project_root / "tests" / "llm-tests" / "generated"
        
        # Create directories
        self.results_dir.mkdir(parents=True, exist_ok=True)
        self.generated_dir.mkdir(parents=True, exist_ok=True)
        
    def extract_cns_code(self, llm_output: str) -> str:
        """Extract CNS code from LLM output, removing markdown/explanations"""
        # Try to find code between ```cns or ```cnsc and ```
        for marker in ["```cnsc", "```cns"]:
            if marker in llm_output:
                start = llm_output.find(marker) + len(marker)
                # Skip to next line (after language identifier)
                newline = llm_output.find("\n", start)
                if newline != -1:
                    start = newline + 1
                end = llm_output.find("```", start)
                if end != -1:
                    return llm_output[start:end].strip()
        
        # Try generic code blocks
        if "```" in llm_output:
            start = llm_output.find("```") + 3
            # Skip language identifier if present (up to newline)
            newline = llm_output.find("\n", start)
            if newline != -1:
                start = newline + 1
            end = llm_output.find("```", start)
            if end != -1:
                return llm_output[start:end].strip()
        
        # Return as-is if no code blocks found
        return llm_output.strip()
    
    def validate_cns(self, code: str) -> Tuple[bool, str]:
        """Validate CNS code using cns-validate"""
        # Write to temp file
        temp_file = self.generated_dir / "temp_validate.cns"
        temp_file.write_text(code)
        
        try:
            result = subprocess.run(
                [str(self.validator_path), str(temp_file)],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            success = result.returncode == 0
            output = result.stdout + result.stderr
            
            return success, output
        except subprocess.TimeoutExpired:
            return False, "Validation timeout"
        except Exception as e:
            return False, f"Validation error: {str(e)}"
    
    def execute_cns(self, code: str, timeout: int = 5) -> Tuple[bool, str]:
        """Execute CNS code using cns-run"""
        temp_file = self.generated_dir / "temp_execute.cns"
        # Ensure file ends with newline (parser requires it)
        if not code.endswith('\n'):
            code += '\n'
        temp_file.write_text(code)
        
        try:
            result = subprocess.run(
                [str(self.runner_path), str(temp_file)],
                capture_output=True,
                text=True,
                timeout=timeout
            )
            
            success = result.returncode == 0
            output = result.stdout + result.stderr
            
            return success, output
        except subprocess.TimeoutExpired:
            return False, "Execution timeout"
        except Exception as e:
            return False, f"Execution error: {str(e)}"
    
    def save_result(self, test_name: str, result_data: Dict) -> Path:
        """Save test results to JSON file"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"{test_name}_{timestamp}.json"
        filepath = self.results_dir / filename
        
        with open(filepath, 'w') as f:
            json.dump(result_data, indent=2, fp=f)
        
        return filepath
    
    def save_generated_code(self, test_name: str, iteration: int, code: str) -> Path:
        """Save generated CNS code"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"{test_name}_iter{iteration}_{timestamp}.cns"
        filepath = self.generated_dir / filename
        # Ensure file ends with newline (parser requires it)
        if not code.endswith('\n'):
            code += '\n'
        filepath.write_text(code)
        return filepath


def create_llm_client(provider: str, api_key: str, model: str) -> LLMClient:
    """Factory function to create appropriate LLM client"""
    if provider == "grok" or provider == "xai":
        return GrokClient(api_key, model)
    elif provider == "openai":
        return OpenAIClient(api_key, model)
    elif provider == "claude" or provider == "anthropic":
        return ClaudeClient(api_key, model)
    elif provider == "openrouter":
        return OpenRouterClient(api_key, model)
    else:
        raise ValueError(f"Unknown provider: {provider}")


def build_prompt_from_syntax(syntax_path: Path, task: str) -> str:
    """Build prompt using SYNTAX.md as single source of truth"""
    if not syntax_path.exists():
        raise FileNotFoundError(f"SYNTAX.md not found at: {syntax_path}")
    
    # Read SYNTAX.md (already has {TASK} placeholder)
    syntax_content = syntax_path.read_text()
    
    # Replace {TASK} with actual task
    return syntax_content.replace("{TASK}", task)


def run_test(
    llm: LLMClient,
    tester: CNSTester,
    prompt: str,
    system_prompt: Optional[str],
    test_name: str,
    max_retries: int = 3,
    verbose: bool = True
) -> Dict:
    """Run a single test with optional retries"""
    
    result = {
        "test_name": test_name,
        "model": llm.model,
        "timestamp": datetime.now().isoformat(),
        "prompt": prompt,
        "system_prompt": system_prompt,
        "attempts": [],
        "success": False,
        "total_attempts": 0
    }
    
    for attempt in range(1, max_retries + 1):
        if verbose:
            print(f"\n{'='*60}")
            print(f"Attempt {attempt}/{max_retries}")
            print(f"{'='*60}")
        
        attempt_data = {
            "attempt_number": attempt,
            "validation_passed": False,
            "execution_passed": False
        }
        
        try:
            # Generate code
            if verbose:
                print("Generating code...")
            
            start_time = time.time()
            llm_output = llm.generate(prompt, system_prompt=system_prompt)
            generation_time = time.time() - start_time
            
            attempt_data["generation_time"] = generation_time
            attempt_data["raw_output"] = llm_output
            
            # Extract CNS code
            cns_code = tester.extract_cns_code(llm_output)
            attempt_data["cns_code"] = cns_code
            
            if verbose:
                print(f"Generated in {generation_time:.2f}s")
                print(f"\nCode length: {len(cns_code)} chars")
            
            # Save generated code
            code_path = tester.save_generated_code(test_name, attempt, cns_code)
            attempt_data["code_file"] = str(code_path)
            
            # Validate
            if verbose:
                print("Validating...")
            
            valid, validation_output = tester.validate_cns(cns_code)
            attempt_data["validation_passed"] = valid
            attempt_data["validation_output"] = validation_output
            
            if verbose:
                if valid:
                    print("✓ VALIDATION PASSED")
                else:
                    print("✗ VALIDATION FAILED")
                    print(f"  {validation_output[:200]}")
            
            if not valid:
                # Add validation error to prompt for retry
                prompt += f"\n\nPrevious attempt failed validation with error:\n{validation_output}\n\nPlease fix and try again."
                result["attempts"].append(attempt_data)
                continue
            
            # Execute
            if verbose:
                print("Executing...")
            
            executed, execution_output = tester.execute_cns(cns_code)
            attempt_data["execution_passed"] = executed
            attempt_data["execution_output"] = execution_output
            
            if verbose:
                if executed:
                    print("✓ EXECUTION PASSED")
                    print(f"\nOutput:\n{execution_output}")
                else:
                    print("✗ EXECUTION FAILED")
                    print(f"  {execution_output[:200]}")
            
            result["attempts"].append(attempt_data)
            
            if valid and executed:
                result["success"] = True
                result["total_attempts"] = attempt
                if verbose:
                    print(f"\n🎉 SUCCESS on attempt {attempt}!")
                break
            
            # Add execution error to prompt for retry
            if not executed:
                prompt += f"\n\nPrevious attempt passed validation but failed execution:\n{execution_output}\n\nPlease fix and try again."
        
        except Exception as e:
            attempt_data["error"] = str(e)
            result["attempts"].append(attempt_data)
            if verbose:
                print(f"✗ ERROR: {e}")
    
    result["total_attempts"] = len(result["attempts"])
    
    # Save results
    result_path = tester.save_result(test_name, result)
    if verbose:
        print(f"\n{'='*60}")
        print(f"Results saved to: {result_path}")
        print(f"{'='*60}")
    
    return result


def load_env_file(project_root: Path):
    """Load environment variables from .env file"""
    env_file = project_root / ".env"
    if env_file.exists():
        with open(env_file) as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith('#') and '=' in line:
                    key, value = line.split('=', 1)
                    os.environ.setdefault(key.strip(), value.strip())


def main():
    parser = argparse.ArgumentParser(
        description="Test LLMs on CNS code generation using SYNTAX.md",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Test Grok on factorial calculation
  %(prog)s --task "Calculate factorial of 10"
  
  # Test with Claude on web server
  %(prog)s --task "Build HTTP server on port 8080" --provider claude
  
  # Quick test with custom name
  %(prog)s --task "Sum numbers 1 to 100" --name sum-range
        """
    )
    parser.add_argument("--task", required=True, help="Task description for CNS program")
    parser.add_argument("--name", help="Test name (defaults to task)")
    parser.add_argument("--provider", default="grok", 
                       choices=["grok", "xai", "openai", "claude", "anthropic", "openrouter"],
                       help="LLM provider (default: grok)")
    parser.add_argument("--model", help="Model to use (provider-specific)")
    parser.add_argument("--retries", type=int, default=3, help="Max retry attempts (default: 3)")
    parser.add_argument("--api-key", help="API key (or set in .env)")
    parser.add_argument("--timeout", type=int, default=5, help="Execution timeout in seconds (default: 5)")
    parser.add_argument("--quiet", action="store_true", help="Minimal output")
    
    args = parser.parse_args()
    
    # Setup project root and load .env
    project_root = Path(__file__).parent.parent
    load_env_file(project_root)
    
    # Determine API key based on provider
    api_key = args.api_key
    if not api_key:
        if args.provider in ["grok", "xai"]:
            api_key = os.environ.get("GROK_API_KEY") or os.environ.get("XAI_API_KEY")
        elif args.provider == "openai":
            api_key = os.environ.get("OPENAI_API_KEY")
        elif args.provider in ["claude", "anthropic"]:
            api_key = os.environ.get("ANTHROPIC_API_KEY") or os.environ.get("CLAUDE_API_KEY")
        elif args.provider == "openrouter":
            api_key = os.environ.get("OPENROUTER_API_KEY")
    
    if not api_key:
        print(f"ERROR: No API key provided for {args.provider}.")
        print(f"  Option 1: Add to .env file")
        print(f"  Option 2: Use --api-key command line argument")
        sys.exit(1)
    
    # Determine model
    model = args.model
    if not model:
        defaults = {
            "grok": "grok-2-latest",
            "xai": "grok-2-latest",
            "openai": "gpt-4",
            "claude": "claude-3-sonnet-20240229",
            "anthropic": "claude-3-sonnet-20240229",
            "openrouter": "anthropic/claude-3-sonnet"
        }
        model = defaults.get(args.provider, "grok-2-latest")
    
    test_name = args.name or args.task.replace(" ", "-").lower()
    
    # Build prompt from SYNTAX.md (single source of truth)
    syntax_path = project_root / "SYNTAX.md"
    
    try:
        prompt = build_prompt_from_syntax(syntax_path, args.task)
        system_prompt = None  # SYNTAX.md is comprehensive, no separate system prompt needed
    except FileNotFoundError as e:
        print(f"ERROR: {e}")
        print(f"Make sure SYNTAX.md exists at project root: {project_root}")
        sys.exit(1)
    
    # Initialize clients
    llm = create_llm_client(args.provider, api_key, model)
    tester = CNSTester(project_root)
    
    print(f"\n{'='*60}")
    print(f"CNS LLM Test Harness")
    print(f"{'='*60}")
    print(f"Task: {args.task}")
    print(f"Test name: {test_name}")
    print(f"Provider: {args.provider}")
    print(f"Model: {model}")
    print(f"Max retries: {args.retries}")
    print(f"Using: SYNTAX.md (single source of truth)")
    
    # Run test
    result = run_test(
        llm=llm,
        tester=tester,
        prompt=prompt,
        system_prompt=system_prompt,
        test_name=test_name,
        max_retries=args.retries,
        verbose=not args.quiet
    )
    
    # Summary
    print(f"\n{'='*60}")
    print("SUMMARY")
    print(f"{'='*60}")
    print(f"Success: {result['success']}")
    print(f"Attempts: {result['total_attempts']}")
    
    if result['success']:
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
