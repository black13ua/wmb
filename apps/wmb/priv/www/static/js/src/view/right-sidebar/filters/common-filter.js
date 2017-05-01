import React, { PropTypes } from 'react';
import classnames from 'classnames';
import CSSModules from 'react-css-modules';
import { alert, listGroup, badge } from 'bootstrap-css';
import translate from '../../../constants/names';

const styles = {};
Object.assign(styles, alert, listGroup, badge);


const FilterView = ({ children, optionsLength, alias, onClick, activeClass }) => {
    const styleName = classnames('list-group-item', { active: activeClass });
    return (
        <div styleName="list-group">
            <button
                className="button--hover"
                styleName={styleName}
                onClick = {onClick}
            >
                <span styleName="badge">{`${optionsLength || '?'}`}</span>
                {`${translate.filterHeaders[alias] || alias}`}
            </button>
            <div className="second--sub--menu">
                { children }
            </div>
        </div>
    );
};


FilterView.propTypes = {
    activeClass  : PropTypes.bool,
    alias        : PropTypes.string.isRequired,
    children     : PropTypes.node,
    optionsLength: PropTypes.number.isRequired,
    onClick      : PropTypes.func.isRequired,
};

export default CSSModules(FilterView, styles, { allowMultiple: true });
