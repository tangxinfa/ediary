#!/bin/env node

process.env.NODE_CONFIG_DIR = __dirname + '/config';

var yargs          = require('yargs'),
    fs             = require('fs'),
    path           = require('path'),
    url            = require('url'),
    sprintf        = require('sprintf-js').sprintf,
    vsprintf       = require('sprintf-js').vsprintf,
    async          = require('async'),
    handlebars     = require('handlebars'),
    mkdirp         = require("mkdirp"),
    config         = require('config'),
    Feed           = require('feed'),
    Sitemap        = require('sitemap').Sitemap,
    tracer         = require('tracer'),
    dateformat     = require('dateformat');


/// Configure logger.
var logger = tracer.console({
    format : "{{timestamp}} <{{title}}>\t{{message}}\t-- {{file}}:{{line}}",
    dateformat: config.logger.timestamp.format,
    level: config.logger.level
});

/**
 * Convert a string to a path.
 *
 * @param str String to convert.
 *
 * @return Converted string.
 */
function pathify (str) {
    var result = '';
    var ascii;

    for(var i = 0, n = str.length; i < n; ++i) {
        if (str.charCodeAt(i) <= 127) {
            if (ascii === false) {
                result += '-';  /// Put a "-" separator between ascii and none-ascii characters.
            }
            ascii = true;
            if ((str[i] >= 'a' && str[i] <= 'z') || str[i] === '-' || str[i] === '_') {
                result += str[i]; /// Characters lowercase underline and midline keep unchanged.
            } else if (str[i] >= 'A' && str[i] <= 'Z') {
                result += str[i].toLowerCase(); /// Convert uppercase characters to lowercase.
            } else {
                result += '-';  /// Convert other ascii characters as "-" separator.
            }
        } else {
            if (ascii === true) {
                result += '-'; /// Put a "-" separator between ascii and none-ascii characters.
            }
            ascii = false;
            result += sprintf('%02x', str.charCodeAt(i)); /// Hex codec none ascii characters.
        }
    }

    result = result.replace(/[-_]{2,}/g, '-');  /// Replace multiple separators to one.
    result = result.replace(/^[-_]+/g, '');     /// Ignore separators at the beginning.
    result = result.replace(/[-_]+$/g, '');     /// Ignore separators at the end.

    return result;
}

/**
 * Get absolute path from relative path.
 *
 * @param relativePath relative path.
 *
 * @return absolute path.
 */
function absolutePath(relativePath) {
    return config.site.link + relativePath;
}

/**
 * Get relative path of tag's page.
 *
 * @param tag tag to get relative path.
 * @param page page to get relative path.
 *
 * @return relative path of tag's page.
 */
function tagPageRelativePath(tag, page) {
    return sprintf("/tag/%s/%d.html", pathify((config.site.tags[tag] && config.site.tags[tag].slug) || tag), page);
}

/**
 * Get relative path of tag's feed.
 *
 * @param tag tag to get relative path.
 *
 * @return relative path of tag's feed.
 */
function tagFeedRelativePath(tag) {
    return sprintf("/tag/%s.xml", pathify((config.site.tags[tag] && config.site.tags[tag].slug) || tag));
}

/**
 * Get relative path of category's page.
 *
 * @param category category to get relative path.
 * @param page page to get relative path.
 *
 * @return relative path of category's page.
 */
function categoryPageRelativePath(category, page) {
    return sprintf("/category/%s/%d.html", pathify((config.site.categories[category] && config.site.categories[category].slug) || category), page);
}

/**
 * Get relative path of category's feed.
 *
 * @param category category to get relative path.
 *
 * @return relative path of category's feed.
 */
function categoryFeedRelativePath(category) {
    return sprintf("/category/%s.xml", pathify((config.site.categories[category] && config.site.categories[category].slug) || category));
}

/**
 * Get relate tags of tag.
 *
 * @param tag tag to get relate tags.
 *
 * @return relate tags.
 */
var relateTags = (function () {
    var cache = {};
    return function (tag) {
        if (! cache[tag]) {
            var tags = [];
            var categories = tagCategories(tag);
            for (var otherTag in ediary.tagArticles) {
                if (otherTag != tag) {
                    var otherCategories = tagCategories(otherTag);
                    categories.forEach(function (category) {
                        if (otherCategories.indexOf(category) !== -1) {
                            if(tags.indexOf(otherTag) === -1) {
                                tags.push(otherTag);
                            }
                        }
                    });
                }
            }
            cache[tag] = tags;
        }

        return cache[tag];
    };
})();


/// Parse command line arguments.
var argv = yargs
    .option('f', {
        alias : 'file',
        demand: true,
        default: './ediary.json',
        describe: 'exported ediary data file',
        type: 'string'
    })
    .usage('Usage: ' + process.argv[0] + ' ' + process.argv[1] + ' [options]')
    .example(process.argv[0] + ' ' + process.argv[1] + ' -f ./ediary.json', 'Publish ediary ./ediary.json')
    .help('h')
    .alias('h', 'help')
    .epilog('copyright 2015')
    .argv;


/// Register template helpers.
handlebars.registerHelper('sprintf', function() {
    if (arguments.length === 1) {
        return arguments[0];
    } else if (arguments.length > 1) {
        return vsprintf(arguments[0], Array.prototype.slice.call(arguments, 1));
    }
});
handlebars.registerHelper('contains', function(arr, key) {
    return Array.isArray(arr) && arr.indexOf(key) !== -1;
});
handlebars.registerHelper('keys', function(obj) {
    return Object.keys(obj);
});
handlebars.registerHelper('field', function(arr, n, name) {
    return arr[n][name];
});
handlebars.registerHelper('dateformat', function (date, format) {
    return dateformat(date, format);
});
handlebars.registerHelper('page', function (articles, page, block) {
    var content = '';
    for(var i = page*config.pager.size, n = Math.min((page+1)*config.pager.size, articles.length); i < n; ++i) {
        content += block.fn(articles[i]);
    }
    return content;
});
handlebars.registerHelper('equal', function (a, b) {
    return a == b;
});
handlebars.registerHelper('tagPageRelativePath', tagPageRelativePath);
handlebars.registerHelper('tagFeedRelativePath', tagFeedRelativePath);
handlebars.registerHelper('categoryPageRelativePath', categoryPageRelativePath);
handlebars.registerHelper('categoryFeedRelativePath', categoryFeedRelativePath);
handlebars.registerHelper('relateTags', relateTags);


/**
 * Get tag's categories.
 *
 * @param tag tag to get categories.
 *
 * @return categories.
 */
var tagCategories = (function () {
    var cache = {};
    return function (tag) {
        if (! cache[tag]) {
            var categories = [];
            var defaultCategories = [];
            for (var category in config.site.categories) {
                if (config.site.categories[category].default && defaultCategories.indexOf(category) === -1) {
                    defaultCategories.push(category);
                }
                if (config.site.categories[category].tags.indexOf(tag) !== -1 && categories.indexOf(category) === -1) {
                    categories.push(category);
                }
            }
            cache[tag] = categories.length > 0 ? categories : defaultCategories;
            if (categories.length === 0) {
                logger.warn("tag " + tag + " use default categories: " + defaultCategories.toString());
            }
        }
        return cache[tag];
    };
})();

/**
 * Parse ediary file.
 *
 * @param filename ediary file name.
 *
 * @return parsed data:
 *         {
 *           allArticles: [Article ...],
 *           tagArticles: {tag: [articleIndex ...] ...},
 *           categoryArticles: {category: [articleIndex ...] ...}
 *         }
 */
function parseEdiaryFile(filename) {
    var result = {
        allArticles: [],
        tagArticles: {},
        categoryArticles: {}
    };
    /// Load ediary file.
    var ediary = require(filename);
    /// Sort ediary entries by timestamp desc.
    ediary.sort(function(e1, e2){
        if (! (e1.timestamp instanceof Date)) {
            e1.timestamp = new Date(e1.timestamp);
        }
        if (! (e2.timestamp instanceof Date)) {
            e2.timestamp = new Date(e2.timestamp);
        }
        return e2.timestamp - e1.timestamp;
    });
    ediary.forEach(function (entry) {
        entry.url = sprintf('/article/%s/%s.html', dateformat(entry.timestamp, 'yyyy/mm/dd'), pathify(entry.title));
        if (! Array.isArray(entry.tags)) {
            entry.tags = [];
        }
        var index = result.allArticles.push(entry) - 1;
        entry.tags.forEach(function (tag) {
            if (! result.tagArticles[tag]) {
                result.tagArticles[tag] = [];
            }
            if (result.tagArticles[tag].indexOf(index) === -1) {
                result.tagArticles[tag].push(index);
            }
            var categories = tagCategories(tag);
            categories.forEach(function (category) {
                if (! result.categoryArticles[category]) {
                    result.categoryArticles[category] = [];
                }
                if (result.categoryArticles[category].indexOf(index) === -1) {
                    result.categoryArticles[category].push(index);
                }
            });
        });
    });

    return result;
}


/**
 * Load template from file.
 *
 * @param file template file.
 * @param callback callback function. function (err, template)
 *
 */
var loadTemplate = (function () {
    var cache = {};
    return function (file, callback) {
        if (cache[file]) {
            return callback(null, cache[file]);
        }

        fs.readFile(file, {encoding: 'utf-8'}, function (err, data) {
            if (err) {
                return callback(err);
            }

            try {
                cache[file] = handlebars.compile(data);
            } catch (e) {
                logger.error("Compile template file " + file + " failed");
                return callback(e);
            }

            logger.log("Debug: " + "Load template file " + file);

            return callback(null, cache[file]);
        });
    };
})();


/**
 * Register template partials.
 *
 * @param callback callback function. function (err)
 *
 */
function registerPartials(callback) {
    logger.debug("register template partials");
    var partialsDir = __dirname + "/templates/partials";
    fs.readdir(partialsDir, function (err, files) {
        if (err) {
            return callback(err);
        }

        async.forEachOfSeries(
            files,
            function (file, index, done) {
                file = path.join(partialsDir, file);

                var extName = path.extname(file);
                if (extName != '.html') {
                    logger.warn('ignore file in partials dir: ' + file);
                    return done(null);
                }

                var partialName = path.basename(file, extName);
                if (! partialName) {
                    logger.warn('ignore file in partials dir: ' + file);
                    return done(null);
                }

                fs.readFile(file, {encoding: 'utf-8'}, function (err, data) {
                    if (err) {
                        return done(err);
                    }

                    try {
                        handlebars.registerPartial(partialName, data);
                    } catch (e) {
                        logger.error("register partial file " + file + " failed");
                        return callback(e);
                    }

                    return done(null);
                });
            },
            callback
        );
    });
}

/**
 * Generate all article pages.
 *
 * @param callback function called when done. function (err)
 *
 */
function generateAllArticlePages(callback) {
    logger.debug("generate all article pages");
    async.forEachOfSeries(
        ediary.allArticles,
        function (entry, index, done) {
            index = +index;

            var file = '.' + entry.url;
            loadTemplate(__dirname + '/templates/article.html', function (err, template) {
                if (err) {
                    return done(err);
                }

                var data = {
                    "title": entry.title,
                    "current": entry,
                    "config": config,
                    "ediary": ediary
                };

                var html = template(data);
                mkdirp(path.dirname(file), function (err) {
                    if (err) {
                        return done(err);
                    }

                    fs.writeFile(file, html, done);
                    logger.info("generated article page: " + file);
                });
            });
        },
        callback
    );
}

/**
 * Generate article pages of tag.
 *
 * @param callback function called when done. function (err)
 *
 */
function generateTagArticlePages(callback) {
    logger.debug("generate tag article pages");

    loadTemplate(__dirname + '/templates/tag.html', function (err, template) {
        if (err) {
            return callback(err);
        }

        async.forEachOfSeries(
            ediary.tagArticles,
            function (articles, tag, done) {
                var pages = [];
                for (var i = 0, n = articles.length; i < n; i += config.pager.size) {
                    pages.push(pages.length);
                }

                async.eachSeries(
                    pages,
                    function (page, done) {
                        var data = {
                            "title": tag,
                            "tag": tag,
                            "articles": articles,
                            "ediary": ediary,
                            "previous": ((page+1)*config.pager.size<articles.length ? tagPageRelativePath(tag, page + 1) : undefined),
                            "page": page,
                            "next": (page > 0 ? tagPageRelativePath(tag, page - 1) : undefined),
                            "config": config
                        };

                        var file = "." + tagPageRelativePath(tag, page);
                        var html = template(data);
                        mkdirp(path.dirname(file), function (err) {
                            if (err) {
                                return done(err);
                            }

                            fs.writeFile(file, html, done);
                            logger.info("generated tag " + tag + " articles page: " + file);
                        });
                    },
                    done);
            },
            callback);
    });
}

/**
 * Generate tag articles feed page.
 *
 * @param callback function called when done. function (err)
 *
 */
function generateTagArticleFeeds(callback) {
    logger.debug("Generate tag articles feed page");

    async.forEachOfSeries(
        ediary.tagArticles,
        function (articles, tag, done) {
            var feed = new Feed({
                title:       tag + ' - ' + config.site.title,
                description: config.site.description,
                link:        absolutePath(tagFeedRelativePath(tag)),
                image:       config.site.image,
                copyright:   config.site.copyright,
                author:      config.author
            });

            var categories = tagCategories(tag);
            categories.forEach(function (category) {
                if (feed.categories.indexOf(category) === -1) {
                    feed.addCategory(category);
                }
            });

            articles.forEach(function (index) {
                feed.addItem({
                    title: ediary.allArticles[index].title,
                    link: ediary.allArticles[index].url,
                    description: "",
                    content: ediary.allArticles[index].body,
                    author: config.author,
                    date: ediary.allArticles[index].timestamp
                });
            });

            var file = '.' + tagFeedRelativePath(tag);
            var xml = feed.render('rss-2.0');
            mkdirp(path.dirname(file), function (err) {
                if (err) {
                    return done(err);
                }

                fs.writeFile(file, xml, function (err) {
                    if (err) {
                        return done(err);
                    }
                    logger.info("generated tag feed page: " + file);
                    done(null);
                });
            });
        },
        callback);
}

function generateCategoryArticlePages(callback) {
    logger.debug("generate category article pages");

    loadTemplate(__dirname + '/templates/category.html', function (err, template) {
        if (err) {
            return callback(err);
        }

        async.forEachOfSeries(
            ediary.categoryArticles,
            function (articles, category, done) {
                var pages = [];
                for (var i = 0, n = articles.length; i < n; i += config.pager.size) {
                    pages.push(pages.length);
                }

                async.eachSeries(
                    pages,
                    function (page, done) {
                        var data = {
                            "title": category,
                            "category": category,
                            "articles": articles,
                            "ediary": ediary,
                            "previous": (page > 0 ? categoryPageRelativePath(category, page - 1) : undefined),
                            "page": page,
                            "next": ((page+1)*config.pager.size<articles.length ? categoryPageRelativePath(category, page + 1) : undefined),
                            "config": config
                        };

                        var file = "." + categoryPageRelativePath(category, page);
                        var html = template(data);
                        mkdirp(path.dirname(file), function (err) {
                            if (err) {
                                return done(err);
                            }

                            fs.writeFile(file, html, done);
                            logger.info("generated category " + category + " articles page: " + file);
                        });
                    },
                    done);
            },
            callback);
    });
}

/**
 * Generate category articles feed page.
 *
 * @param callback function called when done. function (err)
 *
 */
function generateCategoryArticleFeeds(callback) {
    logger.debug("Generate category articles feed page");

    async.forEachOfSeries(
        ediary.categoryArticles,
        function (articles, category, done) {
            var feed = new Feed({
                title:       category + ' - ' + config.site.title,
                description: config.site.description,
                link:        absolutePath(categoryFeedRelativePath(category)),
                image:       config.site.image,
                copyright:   config.site.copyright,
                author:      config.author
            });

            feed.addCategory(category);

            articles.forEach(function (index) {
                feed.addItem({
                    title: ediary.allArticles[index].title,
                    link: ediary.allArticles[index].url,
                    description: "",
                    content: ediary.allArticles[index].body,
                    author: config.author,
                    date: ediary.allArticles[index].timestamp
                });
            });

            var file = '.' + categoryFeedRelativePath(category);
            var xml = feed.render('rss-2.0');
            mkdirp(path.dirname(file), function (err) {
                if (err) {
                    return done(err);
                }

                fs.writeFile(file, xml, function (err) {
                    if (err) {
                        return done(err);
                    }
                    logger.info("generated category feed page: " + file);
                    done(null);
                });
            });
        },
        callback);
}

function generateSiteFeed(callback) {
    logger.debug("generate site feed file");

    var feed = new Feed({
        title:       config.site.title,
        description: config.site.description,
        link:        config.site.link,
        image:       config.site.image,
        copyright:   config.site.copyright,
        author:      config.author
    });

    for(var category in config.site.categories) {
        if (feed.categories.indexOf(category) === -1) {
            feed.addCategory(category);
        }
    }

    ediary.allArticles.forEach(function (article) {
        feed.addItem({
            title: article.title,
            link: article.url,
            description: "",
            content: article.body,
            author: config.author,
            date: article.timestamp
        });
    });

    var file = './index.xml';
    var xml = feed.render('rss-2.0');
    mkdirp(path.dirname(file), function (err) {
        if (err) {
            return callback(err);
        }

        fs.writeFile(file, xml, function (err) {
            if (err) {
                return callback(err);
            }
            logger.info("generated site feed file: " + file);
            callback(null);
        });
    });
}

function generateSiteMap(callback) {
    logger.debug("generate site map file");

    var urls = [];
    ediary.allArticles.forEach(function (article) {
        urls.push(absolutePath(article.url));
    });

    var sitemap = new Sitemap(urls, url.parse(config.site.link).hostname, config.site.cacheTime || (3*60*60));
    var file = './sitemap.xml';
    mkdirp(path.dirname(file), function (err) {
        if (err) {
            return callback(err);
        }

        fs.writeFile(file, sitemap.toXML(), function (err) {
            if (err) {
                return callback(err);
            }
            logger.info("generated site map file: " + file);
            callback(null);
        });
    });
}


/// Fulfill config defaults.
for (var category in config.site.categories) {
    if (! config.site.categories[category].title) {
        config.site.categories[category].title = category;
    }
    if (! config.site.categories[category].slug) {
        config.site.categories[category].slug = config.site.categories[category].title;
    }
    if (! config.site.categories[category].link) {
        config.site.categories[category].link = categoryPageRelativePath(category, 0);
    }
    var defaultIcon = {
        class: "icon-folder-close"
    };
    if (! config.site.categories[category].icon) {
        config.site.categories[category].icon = defaultIcon;
    } else {
        if (! config.site.categories[category].icon.class) {
            config.site.categories[category].icon.class = defaultIcon.class;
        }
    }
    if (! Array.isArray(config.site.categories[category].tags)) {
        config.site.categories[category].tags = [];
    }
}

var ediary = parseEdiaryFile(argv.f);
async.series([
    registerPartials,
    generateAllArticlePages,
    generateTagArticlePages,
    generateTagArticleFeeds,
    generateCategoryArticlePages,
    generateCategoryArticleFeeds,
    generateSiteFeed,
    generateSiteMap,
], function (err) {
    if (err) {
        logger.error(err.toString());
        process.exit(1);
    }
});
