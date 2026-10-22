/**
 * Local semantic-release plugin for an R package.
 *
 * semantic-release has no built-in notion of an R package, so this plugin
 * handles the two R-specific release steps:
 *
 *   1. Bump the `Version:` field in DESCRIPTION to the next release version
 *   2. Prepend the generated release notes to NEWS.md under a
 *      `# <package> <version>` heading, which is the format pkgdown expects
 *
 * If NEWS.md starts with a development heading (`# sportyR`,
 * `# sportyR (development version)`, or a heading for the version being
 * released), any hand-written notes beneath it are kept and placed above the
 * generated notes in the new release section.
 *
 * Writing the files here (rather than via @semantic-release/exec) avoids
 * passing commit-message text through a shell.
 */
const fs = require("fs");
const path = require("path");

const DESCRIPTION_FILE = "DESCRIPTION";
const NEWS_FILE = "NEWS.md";

/**
 * Read the package name from DESCRIPTION.
 *
 * @param {string} description Contents of the DESCRIPTION file
 * @returns {string} The value of the `Package:` field
 */
function packageName(description) {
    const match = description.match(/^Package:\s*(\S+)\s*$/m);
    if (!match) {
        throw new Error(`No "Package:" field found in ${DESCRIPTION_FILE}`);
    }
    return match[1];
}

/**
 * Convert semantic-release's generated notes into pkgdown-friendly markdown.
 *
 * The first line of the generated notes is a version/compare-link heading,
 * which is replaced by the `# <package> <version>` heading, so it is dropped.
 * Section headings (`### Features`, `### Bug Fixes`, ...) are promoted to
 * `##` so they nest directly under the version heading.
 *
 * @param {string} notes Release notes from the release-notes-generator
 * @returns {string} Reformatted notes, or "" if there are none
 */
function formatNotes(notes) {
    const lines = (notes || "").trim().split("\n");
    if (lines.length > 0 && /^#+\s/.test(lines[0])) {
        lines.shift();
    }
    return lines
        .join("\n")
        .replace(/^#{3,}\s+/gm, "## ")
        .trim();
}

/**
 * Split NEWS.md into any hand-written development notes and the rest.
 *
 * A leading heading for the version being released (e.g. a manual
 * `# sportyR 2.3.0` written before the release ran) is treated the same as a
 * development heading, so its notes are merged rather than duplicated.
 *
 * @param {string} news Contents of NEWS.md
 * @param {string} pkg The package name
 * @param {string} version The version being released
 * @returns {{devNotes: string, rest: string}} Notes found under a leading
 *     development heading (or "" if there is none), and the remaining text
 */
function splitDevelopmentSection(news, pkg, version) {
    const escapedVersion = version.replace(/\./g, "\\.");
    // Matches "# pkg", "# pkg ", "# pkg (development version)" and
    // "# pkg <version>" on the first non-blank line, then lazily captures
    // everything up to the next top-level heading (or end of file)
    const devPattern = new RegExp(
        `^\\s*#\\s+${pkg}(?:\\s+\\(development version\\)|\\s+${escapedVersion})?[ \\t]*\\n([\\s\\S]*?)(?=^#\\s|(?![\\s\\S]))`,
        "m"
    );
    const match = news.match(devPattern);
    if (!match || match.index !== 0) {
        return { devNotes: "", rest: news };
    }
    return {
        devNotes: match[1].trim(),
        rest: news.slice(match[0].length)
    };
}

/**
 * Check that the files this plugin edits exist before a release starts.
 *
 * @param {object} pluginConfig Plugin options from .releaserc.json (unused)
 * @param {object} context semantic-release context
 */
function verifyConditions(pluginConfig, context) {
    const descriptionPath = path.join(context.cwd, DESCRIPTION_FILE);
    if (!fs.existsSync(descriptionPath)) {
        throw new Error(`${DESCRIPTION_FILE} not found in ${context.cwd}`);
    }
    const description = fs.readFileSync(descriptionPath, "utf8");
    packageName(description);
    if (!/^Version:/m.test(description)) {
        throw new Error(`No "Version:" field found in ${DESCRIPTION_FILE}`);
    }
}

/**
 * Bump DESCRIPTION and prepend the release section to NEWS.md.
 *
 * @param {object} pluginConfig Plugin options from .releaserc.json (unused)
 * @param {object} context semantic-release context
 */
function prepare(pluginConfig, context) {
    const { cwd, nextRelease, logger } = context;
    const version = nextRelease.version;

    const descriptionPath = path.join(cwd, DESCRIPTION_FILE);
    const description = fs.readFileSync(descriptionPath, "utf8");
    const pkg = packageName(description);
    fs.writeFileSync(
        descriptionPath,
        description.replace(/^Version:.*$/m, `Version: ${version}`)
    );
    logger.log(`Set ${DESCRIPTION_FILE} Version to ${version}`);

    const newsPath = path.join(cwd, NEWS_FILE);
    const news = fs.existsSync(newsPath) ? fs.readFileSync(newsPath, "utf8") : "";
    const { devNotes, rest } = splitDevelopmentSection(news, pkg, version);
    const body = [devNotes, formatNotes(nextRelease.notes)]
        .filter((section) => section.length > 0)
        .join("\n\n");
    const section = `# ${pkg} ${version}\n\n${body}`.trim();
    fs.writeFileSync(newsPath, `${section}\n\n${rest.trimStart()}`);
    logger.log(`Added ${pkg} ${version} section to ${NEWS_FILE}`);
}

module.exports = { verifyConditions, prepare };
