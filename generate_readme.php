<?php

/**
 * Quick and dirty script that appends a ToC to the README based on the
 * directory structure.
 *
 * As a side effect also generates the exercise-specific READMEs which contain
 * gifs of the exercise (if one exists).
 */

const GITHUB_TREE_BASE = 'https://github.com/mark-gerarts/nature-of-code/tree/master/';
const SCREENSHOT_HOST = 'https://raw.githubusercontent.com/mark-gerarts/nature-of-code/master/screenshots/';

function getSubDirectories(string $directory): array {
    $directories =  glob($directory . '/*', GLOB_ONLYDIR);
    return array_map('basename', $directories);
}

function getChapterStructure(): array {
    $excluded_dirs = [
        'screenshots'
    ];

    $dirs = getSubDirectories(__DIR__);
    $dirs = array_filter(
        $dirs,
        function (string $dir) use ($excluded_dirs): bool {
            return !in_array($dir, $excluded_dirs);
        }
    );

    $structure = [];
    foreach ($dirs as $chapter) {
        $exercises = getSubDirectories($chapter);
        $structure[$chapter] = $exercises;
    }

    return $structure;
}

function generateToc(array $structure): string {
    $toc = '';
    foreach ($structure as $chapter => $exercises) {
        $chapter_link = GITHUB_TREE_BASE . rawurlencode($chapter);
        $toc .= "- [$chapter]($chapter_link)\n";

        foreach ($exercises as $exercise) {
            $ex_link = $chapter_link . '/' . rawurlencode($exercise);
            $toc .= "  - [$exercise]($ex_link)\n";
        }
    }

    return $toc;
}

function appendToc(string $toc): void {
    $readme = file(__DIR__ .'/README.md');
    $toc_line = array_search("## Table of contents\n", $readme);
    if (!$toc_line) {
        return;
    }

    $start_line = $toc_line + 1;
    $readme = array_slice($readme, 0, $start_line);

    $toc_lines = explode("\n", $toc);
    $toc_lines = array_map(function (string $line): string {
        return $line . "\n";
    }, $toc_lines);

    $readme = array_merge($readme, $toc_lines);
    $readme = implode("", $readme);
    $readme = trim($readme) . "\n";

    file_put_contents(__DIR__ . '/README.md', $readme);
}

function generateReadmes(array $structure): void {
    $screenshots = glob('screenshots/*');
    $screenshots = array_map('basename', $screenshots);

    foreach ($structure as $chapter => $exercises) {
        foreach ($exercises as $exercise) {
            $readme = $chapter . '/' . $exercise . '/' . 'README.md';
            if (file_exists($readme)) {
                continue;
            }
            if (!in_array($exercise . '.gif', $screenshots)) {
                continue;
            }
            $screenshot_url = SCREENSHOT_HOST . rawurlencode($exercise) . '.gif';

            $contents = "# $exercise\n";
            $contents .= "\n";
            $contents .= "![$exercise]($screenshot_url)";
            file_put_contents($readme, $contents);
        }
    }
}

function main(): void {
    // 1. Output the ToC.
    $chapter_structure = getChapterStructure();
    $toc = generateToc($chapter_structure);
    appendToc($toc);

    // 2. Generate READMEs if needed.
    generateReadmes($chapter_structure);
}

main();
