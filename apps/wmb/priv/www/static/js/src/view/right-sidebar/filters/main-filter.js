import React, { PropTypes } from 'react';

const FiltersView = ({ children }) =>
    <section className="filters">
        { children }
    </section>;

FiltersView.propTypes = {
    children: PropTypes.node.isRequired,
};

export default FiltersView;
