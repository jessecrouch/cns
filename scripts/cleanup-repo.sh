#!/bin/bash
# CNS Repository Cleanup Script
# Maintains repository hygiene by archiving old session docs and organizing test results

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
ARCHIVE_AGE_DAYS=7
DEV_DOCS_DIR="docs/development"
ARCHIVE_DIR="docs/archive"
TESTS_RESULTS_DIR="tests/results"
LARGE_FILE_THRESHOLD=102400  # 100KB in bytes

# Permanent files that should never be archived
PERMANENT_FILES=(
    "LISP-DEBUGGING-GUIDE.md"
    "TESTING.md"
    "ROADMAP.md"
    "README.md"
)

# Mode flags
DRY_RUN=false
AUTO_MODE=false
REPORT_ONLY=false
AGGRESSIVE=false

# Statistics
ARCHIVED_COUNT=0
MOVED_COUNT=0
FLAGGED_COUNT=0

usage() {
    cat << EOF
Usage: $0 [OPTIONS]

CNS Repository Cleanup Script - Maintains repository hygiene

OPTIONS:
    --dry-run       Preview changes without applying them (default)
    --auto          Archive old sessions automatically (applies changes)
    --report        Show size/bloat metrics only
    --aggressive    Remove duplicates and large files (requires confirmation)
    -h, --help      Show this help message

EXAMPLES:
    $0 --dry-run             # Preview what would be cleaned up
    $0 --auto                # Execute cleanup automatically
    $0 --report              # Show repository metrics
    $0 --auto --aggressive   # Full cleanup with duplicate removal

RETENTION POLICY:
    - Session docs older than $ARCHIVE_AGE_DAYS days: Archived
    - Permanent guides: Never archived
    - Test results in root: Moved to tests/results/
    - Files >100KB: Flagged for review
EOF
    exit 0
}

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[!]${NC} $1"
}

log_error() {
    echo -e "${RED}[✗]${NC} $1"
}

is_permanent_file() {
    local filename="$1"
    for perm_file in "${PERMANENT_FILES[@]}"; do
        if [[ "$filename" == "$perm_file" ]]; then
            return 0
        fi
    done
    return 1
}

get_file_age_days() {
    local file="$1"
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS
        local mod_time=$(stat -f %m "$file")
    else
        # Linux
        local mod_time=$(stat -c %Y "$file")
    fi
    local current_time=$(date +%s)
    echo $(( (current_time - mod_time) / 86400 ))
}

get_year_month() {
    local file="$1"
    if [[ "$OSTYPE" == "darwin"* ]]; then
        date -r $(stat -f %m "$file") +%Y-%m
    else
        date -d @$(stat -c %Y "$file") +%Y-%m
    fi
}

show_metrics() {
    log_info "Repository Size Metrics"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    local total_size=$(du -sh . 2>/dev/null | cut -f1)
    local docs_size=$(du -sh docs 2>/dev/null | cut -f1)
    local examples_size=$(du -sh examples 2>/dev/null | cut -f1)
    local src_size=$(du -sh src 2>/dev/null | cut -f1)
    
    echo "Total repository:    $total_size"
    echo "Documentation:       $docs_size"
    echo "Examples:            $examples_size"
    echo "Source code:         $src_size"
    echo ""
    
    local doc_count=$(find docs -name "*.md" 2>/dev/null | wc -l | tr -d ' ')
    local example_count=$(find examples -name "*.cns" 2>/dev/null | wc -l | tr -d ' ')
    local test_count=$(find tests -type f -name "*.cns" -o -name "*.lisp" 2>/dev/null | wc -l | tr -d ' ')
    
    echo "Documentation files: $doc_count"
    echo "Example files:       $example_count"
    echo "Test files:          $test_count"
    echo ""
    
    # Development docs breakdown
    local dev_docs_count=$(find "$DEV_DOCS_DIR" -name "*.md" 2>/dev/null | wc -l | tr -d ' ')
    echo "Development docs:    $dev_docs_count files"
    
    # Large files
    echo ""
    log_info "Large files (>100KB):"
    find . -type f -size +100k ! -path "./.git/*" -exec du -h {} + | sort -rh | head -10
    
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
}

archive_old_sessions() {
    log_info "Scanning for old session documents in $DEV_DOCS_DIR..."
    
    if [[ ! -d "$DEV_DOCS_DIR" ]]; then
        log_error "Directory $DEV_DOCS_DIR not found"
        return 1
    fi
    
    local files_to_archive=()
    
    while IFS= read -r -d '' file; do
        local filename=$(basename "$file")
        
        # Skip permanent files
        if is_permanent_file "$filename"; then
            continue
        fi
        
        # Skip files that are clearly permanent/active
        if [[ "$filename" =~ ^(LLM-IMPROVEMENTS-ROADMAP|LLM-TEMPLATE-STATUS)\.md$ ]]; then
            continue
        fi
        
        # Check age for session files
        if [[ "$filename" =~ SESSION.*\.md$ ]] || [[ "$filename" =~ ^(PHASE-|TEST-RESULTS-).*\.md$ ]]; then
            local age_days=$(get_file_age_days "$file")
            if [[ $age_days -gt $ARCHIVE_AGE_DAYS ]]; then
                files_to_archive+=("$file")
            fi
        fi
    done < <(find "$DEV_DOCS_DIR" -maxdepth 1 -name "*.md" -type f -print0)
    
    if [[ ${#files_to_archive[@]} -eq 0 ]]; then
        log_success "No old session documents found to archive"
        return 0
    fi
    
    log_info "Found ${#files_to_archive[@]} files to archive:"
    for file in "${files_to_archive[@]}"; do
        local filename=$(basename "$file")
        local age_days=$(get_file_age_days "$file")
        echo "  - $filename (${age_days} days old)"
    done
    
    if [[ "$DRY_RUN" == true ]]; then
        log_warning "DRY RUN: Would archive ${#files_to_archive[@]} files"
        return 0
    fi
    
    # Create archive directories and move files
    for file in "${files_to_archive[@]}"; do
        local year_month=$(get_year_month "$file")
        local archive_subdir="$ARCHIVE_DIR/$year_month"
        
        mkdir -p "$archive_subdir"
        
        local filename=$(basename "$file")
        local dest="$archive_subdir/$filename"
        
        if [[ -f "$dest" ]]; then
            log_warning "File already exists in archive: $filename (skipping)"
            continue
        fi
        
        mv "$file" "$dest"
        log_success "Archived: $filename → $archive_subdir/"
        ((ARCHIVED_COUNT++))
    done
}

move_root_test_results() {
    log_info "Checking for test results in root directory..."
    
    local test_files=(
        "TEST-RUN-RESULTS-2025-11-03.md"
        "TEST-STATUS.md"
    )
    
    mkdir -p "$TESTS_RESULTS_DIR"
    
    for file in "${test_files[@]}"; do
        if [[ -f "$file" ]]; then
            if [[ "$DRY_RUN" == true ]]; then
                log_warning "DRY RUN: Would move $file → $TESTS_RESULTS_DIR/"
            else
                mv "$file" "$TESTS_RESULTS_DIR/"
                log_success "Moved: $file → $TESTS_RESULTS_DIR/"
                ((MOVED_COUNT++))
            fi
        fi
    done
}

flag_large_files() {
    log_info "Scanning for large files (>100KB) in docs/development..."
    
    while IFS= read -r -d '' file; do
        local filename=$(basename "$file")
        local size=$(du -h "$file" | cut -f1)
        
        # Skip permanent files
        if is_permanent_file "$filename"; then
            continue
        fi
        
        log_warning "Large file flagged: $filename ($size)"
        echo "  Consider compressing or splitting this file"
        ((FLAGGED_COUNT++))
    done < <(find "$DEV_DOCS_DIR" -maxdepth 1 -name "*.md" -type f -size +${LARGE_FILE_THRESHOLD}c -print0)
}

show_summary() {
    echo ""
    log_info "Cleanup Summary"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "Files archived:      $ARCHIVED_COUNT"
    echo "Files moved:         $MOVED_COUNT"
    echo "Large files flagged: $FLAGGED_COUNT"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    if [[ "$DRY_RUN" == true ]]; then
        echo ""
        log_info "This was a DRY RUN - no changes were made"
        log_info "Run with --auto to apply changes"
    fi
}

main() {
    echo "CNS Repository Cleanup"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    
    if [[ "$REPORT_ONLY" == true ]]; then
        show_metrics
        exit 0
    fi
    
    if [[ "$DRY_RUN" == true ]]; then
        log_info "Running in DRY RUN mode (preview only)"
    else
        log_info "Running in AUTO mode (will make changes)"
    fi
    echo ""
    
    # Show initial metrics
    show_metrics
    echo ""
    
    # Execute cleanup tasks
    archive_old_sessions
    echo ""
    
    move_root_test_results
    echo ""
    
    flag_large_files
    echo ""
    
    # Show summary
    show_summary
    
    if [[ "$DRY_RUN" == false && ($ARCHIVED_COUNT -gt 0 || $MOVED_COUNT -gt 0) ]]; then
        echo ""
        log_info "Final repository size:"
        du -sh . | cut -f1
    fi
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --auto)
            DRY_RUN=false
            AUTO_MODE=true
            shift
            ;;
        --report)
            REPORT_ONLY=true
            shift
            ;;
        --aggressive)
            AGGRESSIVE=true
            shift
            ;;
        -h|--help)
            usage
            ;;
        *)
            log_error "Unknown option: $1"
            usage
            ;;
    esac
done

# Default to dry-run if no mode specified
if [[ "$AUTO_MODE" == false && "$REPORT_ONLY" == false ]]; then
    DRY_RUN=true
fi

main
