module.exports = {
    site: {
        title: '',
        description: '',
        link: '',
        image: '',
        copyright: '',
        tags: {
            //
            // tag: {
            //     slug: string
            // },
            //

        },
        categories: {
            //
            // category: {
            //     default: bool,             // unclassified tags belong to default category
            //     featured: bool,
            //     title: string,             // default is category name
            //     slug: string,              // default is title
            //     icon: {
            //        class: string,          // icon css class, default is "icon-folder-close"
            //        link: string            // icon url
            //     },
            //     tags: [tag ...],           // tags belong to this category.
            // },
            //
            ediary: {
                default: true,
                featured: true
            },
        }
    },
    logger: {
        timestamp: {
            format: "yyyy-mm-dd HH:MM:ss"
        },
        level: 'debug'
    },
    pager: {
        size: 5
    },
    author: {
        name: '',
        email: '',
        link: ''
    },
    disqus: {
        shortname: '' // Specify your registered shortname on disqus
    },
};
