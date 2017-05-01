import React, { PropTypes } from 'react';
import classnames from 'classnames';
import CSSModules from 'react-css-modules';
import { listGroup } from 'bootstrap-css';

const styles = {};
Object.assign(styles, listGroup);

const FilterItemView = ({ children, activeClass, onClick }) => {
    const itemClasses = classnames('list-group-item', { active: activeClass });
    return (
        <button
            className = "button--hover"
            styleName = {itemClasses}
            type      = "button"
            onClick   = {onClick}
        >
            { children }
        </button>
    );
};


FilterItemView.propTypes = {
    activeClass: PropTypes.bool,
    children   : PropTypes.node,
    onClick    : PropTypes.func.isRequired,
};

export default CSSModules(FilterItemView, styles, { allowMultiple: true });
